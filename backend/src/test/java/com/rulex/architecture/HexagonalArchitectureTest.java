package com.rulex.architecture;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;
import static com.tngtech.archunit.library.Architectures.layeredArchitecture;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import org.junit.jupiter.api.Test;

/**
 * Testes arquiteturais para enforçar Clean Architecture / Hexagonal Architecture.
 * 
 * Camadas:
 * - Domain: Lógica de negócio pura, zero dependências externas
 * - Application: Use cases e orquestração, depende apenas de Domain
 * - Infrastructure: Adapters (JPA, Redis, etc), implementa ports do Domain
 * - Interfaces: Controllers REST, depende de Application
 * 
 * @see <a href="https://www.archunit.org/">ArchUnit Documentation</a>
 */
class HexagonalArchitectureTest {

    // Importar classes do projeto (excluindo testes)
    private static final JavaClasses classes = new ClassFileImporter()
            .withImportOption(ImportOption.Predefined.DO_NOT_INCLUDE_TESTS)
            .importPackages("com.rulex");

    /**
     * REGRA 1: Domain não pode depender de outras camadas.
     * Domain deve ser completamente independente (zero Spring, zero JPA, zero Redis).
     */
    @Test
    void domainShouldNotDependOnOtherLayers() {
        noClasses()
                .that().resideInAPackage("..domain..")
                .should().dependOnClassesThat().resideInAnyPackage(
                        "..application..",
                        "..infrastructure..",
                        "..interfaces..",
                        "org.springframework..",
                        "jakarta.persistence.."
                )
                .because("Domain deve ser puro e independente de frameworks")
                .check(classes);
    }

    /**
     * REGRA 2: Application pode depender apenas de Domain.
     * Application não deve depender de Infrastructure ou Interfaces.
     */
    @Test
    void applicationShouldOnlyDependOnDomain() {
        noClasses()
                .that().resideInAPackage("..application..")
                .should().dependOnClassesThat().resideInAnyPackage(
                        "..infrastructure..",
                        "..interfaces.."
                )
                .because("Application deve depender apenas de Domain (ports)")
                .check(classes);
    }

    /**
     * REGRA 3: Infrastructure pode acessar Domain mas não Application ou Interfaces.
     * Infrastructure implementa ports definidos no Domain.
     */
    @Test
    void infrastructureShouldNotDependOnApplicationOrInterfaces() {
        noClasses()
                .that().resideInAPackage("..infrastructure..")
                .should().dependOnClassesThat().resideInAnyPackage(
                        "..application..",
                        "..interfaces.."
                )
                .because("Infrastructure deve implementar ports do Domain, não acessar camadas superiores")
                .check(classes);
    }

    /**
     * REGRA 4: Interfaces (controllers) podem depender de Application e Domain, mas não de Infrastructure.
     * Controllers devem chamar use cases, não acessar repositórios diretamente.
     */
    @Test
    void interfacesShouldNotDependOnInfrastructure() {
        noClasses()
                .that().resideInAPackage("..interfaces..")
                .should().dependOnClassesThat().resideInAPackage("..infrastructure..")
                .because("Controllers devem chamar use cases, não acessar infrastructure diretamente")
                .check(classes);
    }

    /**
     * REGRA 5: Domain models não devem ter anotações JPA.
     * Modelos de domínio devem ser POJOs puros.
     */
    @Test
    void domainModelsShouldNotHaveJpaAnnotations() {
        noClasses()
                .that().resideInAPackage("..domain.model..")
                .should().beAnnotatedWith("jakarta.persistence.Entity")
                .orShould().beAnnotatedWith("jakarta.persistence.Table")
                .orShould().beAnnotatedWith("jakarta.persistence.Id")
                .because("Domain models devem ser POJOs puros, sem anotações de infraestrutura")
                .check(classes);
    }

    /**
     * REGRA 6: Domain models não devem ter anotações Spring.
     */
    @Test
    void domainModelsShouldNotHaveSpringAnnotations() {
        noClasses()
                .that().resideInAPackage("..domain..")
                .should().dependOnClassesThat().resideInAPackage("org.springframework..")
                .because("Domain deve ser livre de frameworks")
                .check(classes);
    }

    /**
     * REGRA 7: Use cases devem estar no pacote application.usecase.
     */
    @Test
    void useCasesShouldResideInApplicationLayer() {
        classes()
                .that().haveSimpleNameEndingWith("UseCase")
                .should().resideInAPackage("..application.usecase..")
                .because("Use cases devem estar na camada de aplicação")
                .check(classes);
    }

    /**
     * REGRA 8: Repositories JPA devem estar na infraestrutura.
     */
    @Test
    void jpaRepositoriesShouldResideInInfrastructure() {
        classes()
                .that().implement("org.springframework.data.jpa.repository.JpaRepository")
                .should().resideInAPackage("..infrastructure.persistence..")
                .because("Repositórios JPA são adapters e devem estar na infrastructure")
                .check(classes);
    }

    /**
     * REGRA 9: Controllers REST devem estar no pacote interfaces.rest.
     */
    @Test
    void restControllersShouldResideInInterfacesLayer() {
        classes()
                .that().areAnnotatedWith("org.springframework.web.bind.annotation.RestController")
                .should().resideInAPackage("..interfaces.rest..")
                .because("Controllers REST são adapters de entrada e devem estar em interfaces")
                .check(classes);
    }

    /**
     * REGRA 10: Arquitetura em camadas (enforcement global).
     */
    @Test
    void layeredArchitectureShouldBeRespected() {
        layeredArchitecture()
                .consideringAllDependencies()
                .layer("Interfaces").definedBy("..interfaces..")
                .layer("Application").definedBy("..application..")
                .layer("Domain").definedBy("..domain..")
                .layer("Infrastructure").definedBy("..infrastructure..")

                .whereLayer("Interfaces").mayNotBeAccessedByAnyLayer()
                .whereLayer("Application").mayOnlyBeAccessedByLayers("Interfaces")
                .whereLayer("Domain").mayOnlyBeAccessedByLayers("Application", "Infrastructure")
                .whereLayer("Infrastructure").mayOnlyBeAccessedByLayers("Interfaces")

                .because("Arquitetura hexagonal deve ser respeitada: fluxo unidirecional de dependências")
                .check(classes);
    }
}
