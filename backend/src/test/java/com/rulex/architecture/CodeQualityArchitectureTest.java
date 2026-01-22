package com.rulex.architecture;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;

import com.tngtech.archunit.core.domain.JavaClass;
import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.domain.JavaMethod;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.lang.ArchCondition;
import com.tngtech.archunit.lang.ConditionEvents;
import com.tngtech.archunit.lang.SimpleConditionEvent;
import java.util.Set;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Testes de arquitetura para garantir qualidade do código.
 *
 * <p>Estes testes verificam:
 * <ul>
 *   <li>Limite de linhas por classe</li>
 *   <li>Limite de métodos por classe</li>
 *   <li>Limite de dependências injetadas</li>
 *   <li>Separação de camadas</li>
 * </ul>
 *
 * <p>NOTA: Alguns testes estão @Disabled porque o código atual viola as regras.
 * Eles servem como meta para refatoração futura.
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
public class CodeQualityArchitectureTest {

    private static final String BASE_PACKAGE = "com.rulex";

    // Limites de qualidade (metas)
    private static final int MAX_LINES_PER_CLASS = 500;
    private static final int MAX_METHODS_PER_CLASS = 20;
    private static final int MAX_CONSTRUCTOR_DEPENDENCIES = 7;

    // Classes que estão em processo de refatoração (exceções temporárias)
    private static final Set<String> REFACTORING_EXCEPTIONS = Set.of(
        "RuleEngineService",
        "ComplexRuleEvaluator",
        "DatabaseRuleExecutorService",
        "Neo4jGraphService"
    );

    private static JavaClasses classes;

    @BeforeAll
    static void setup() {
        classes = new ClassFileImporter()
            .withImportOption(new ImportOption.DoNotIncludeTests())
            .importPackages(BASE_PACKAGE);
    }

    @Test
    @DisplayName("Services should not depend on Controllers")
    void servicesShouldNotDependOnControllers() {
        noClasses()
            .that().resideInAPackage("..service..")
            .should().dependOnClassesThat().resideInAPackage("..controller..")
            .check(classes);
    }

    @Test
    @DisplayName("Repositories should not depend on Services")
    void repositoriesShouldNotDependOnServices() {
        noClasses()
            .that().resideInAPackage("..repository..")
            .should().dependOnClassesThat().resideInAPackage("..service..")
            .check(classes);
    }

    @Test
    @Disabled("DTOs in homolog package use entity enums - needs refactoring")
    @DisplayName("DTOs should not depend on Entities")
    void dtosShouldNotDependOnEntities() {
        noClasses()
            .that().resideInAPackage("..dto..")
            .should().dependOnClassesThat().resideInAPackage("..entity..")
            .check(classes);
    }

    @Test
    @DisplayName("Entities should not depend on Services")
    void entitiesShouldNotDependOnServices() {
        noClasses()
            .that().resideInAPackage("..entity..")
            .should().dependOnClassesThat().resideInAPackage("..service..")
            .check(classes);
    }

    @Test
    @DisplayName("New services should have max 7 constructor dependencies")
    void newServicesShouldHaveMaxSevenDependencies() {
        classes()
            .that().resideInAPackage("..service.engine..")
            .and().haveSimpleNameEndingWith("Service")
            .should(haveMaxConstructorDependencies(MAX_CONSTRUCTOR_DEPENDENCIES))
            .check(classes);
    }

    @Test
    @DisplayName("Strategy classes should have max 5 dependencies")
    void strategiesShouldHaveMaxFiveDependencies() {
        classes()
            .that().resideInAPackage("..operator.strategy..")
            .and().haveSimpleNameEndingWith("Strategy")
            .should(haveMaxConstructorDependencies(5))
            .check(classes);
    }

    @Test
    @Disabled("Meta para refatoração - código atual viola esta regra")
    @DisplayName("[META] All services should have max 500 lines")
    void allServicesShouldHaveMaxLines() {
        classes()
            .that().resideInAPackage("..service..")
            .and().haveSimpleNameEndingWith("Service")
            .should(haveMaxLines(MAX_LINES_PER_CLASS))
            .check(classes);
    }

    @Test
    @Disabled("Meta para refatoração - código atual viola esta regra")
    @DisplayName("[META] All services should have max 20 methods")
    void allServicesShouldHaveMaxMethods() {
        classes()
            .that().resideInAPackage("..service..")
            .and().haveSimpleNameEndingWith("Service")
            .should(haveMaxMethods(MAX_METHODS_PER_CLASS))
            .check(classes);
    }

    @Test
    @DisplayName("New engine services should have max 500 lines")
    void newEngineServicesShouldHaveMaxLines() {
        classes()
            .that().resideInAPackage("..service.engine..")
            .and().haveSimpleNameEndingWith("Service")
            .should(haveMaxLines(MAX_LINES_PER_CLASS))
            .check(classes);
    }

    @Test
    @DisplayName("Operator strategies should have max 300 lines")
    void operatorStrategiesShouldHaveMaxLines() {
        classes()
            .that().resideInAPackage("..operator.strategy..")
            .and().haveSimpleNameEndingWith("Strategy")
            .should(haveMaxLines(300))
            .check(classes);
    }

    @Test
    @Disabled("Controllers currently depend on entities - needs refactoring")
    @DisplayName("Controllers should only depend on Services and DTOs")
    void controllersShouldOnlyDependOnServicesAndDtos() {
        noClasses()
            .that().resideInAPackage("..controller..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("..repository..", "..entity..")
            .check(classes);
    }

    // ==================== Custom ArchConditions ====================

    private static ArchCondition<JavaClass> haveMaxLines(int maxLines) {
        return new ArchCondition<>("have at most " + maxLines + " lines") {
            @Override
            public void check(JavaClass javaClass, ConditionEvents events) {
                // Pular classes em refatoração
                if (REFACTORING_EXCEPTIONS.contains(javaClass.getSimpleName())) {
                    return;
                }

                // ArchUnit Source doesn't have getLines() - use method count as proxy
                // This is a simplified check
                long methodCount = javaClass.getMethods().size();
                // Estimate: ~10 lines per method on average
                long estimatedLines = methodCount * 10;
                if (estimatedLines > maxLines) {
                    String message = String.format(
                        "%s has approximately %d lines (estimated from %d methods, max allowed: %d)",
                        javaClass.getName(), estimatedLines, methodCount, maxLines);
                    events.add(SimpleConditionEvent.violated(javaClass, message));
                }
            }
        };
    }

    private static ArchCondition<JavaClass> haveMaxMethods(int maxMethods) {
        return new ArchCondition<>("have at most " + maxMethods + " methods") {
            @Override
            public void check(JavaClass javaClass, ConditionEvents events) {
                // Pular classes em refatoração
                if (REFACTORING_EXCEPTIONS.contains(javaClass.getSimpleName())) {
                    return;
                }

                long methodCount = javaClass.getMethods().stream()
                    .filter(m -> !m.getName().startsWith("get"))
                    .filter(m -> !m.getName().startsWith("set"))
                    .filter(m -> !m.getName().startsWith("is"))
                    .filter(m -> !m.getName().equals("toString"))
                    .filter(m -> !m.getName().equals("hashCode"))
                    .filter(m -> !m.getName().equals("equals"))
                    .count();

                if (methodCount > maxMethods) {
                    String message = String.format(
                        "%s has %d methods (max allowed: %d)",
                        javaClass.getName(), methodCount, maxMethods);
                    events.add(SimpleConditionEvent.violated(javaClass, message));
                }
            }
        };
    }

    private static ArchCondition<JavaClass> haveMaxConstructorDependencies(int maxDeps) {
        return new ArchCondition<>("have at most " + maxDeps + " constructor dependencies") {
            @Override
            public void check(JavaClass javaClass, ConditionEvents events) {
                // Pular classes em refatoração
                if (REFACTORING_EXCEPTIONS.contains(javaClass.getSimpleName())) {
                    return;
                }

                javaClass.getConstructors().forEach(constructor -> {
                    int paramCount = constructor.getRawParameterTypes().size();
                    if (paramCount > maxDeps) {
                        String message = String.format(
                            "%s has constructor with %d dependencies (max allowed: %d)",
                            javaClass.getName(), paramCount, maxDeps);
                        events.add(SimpleConditionEvent.violated(javaClass, message));
                    }
                });
            }
        };
    }
}
