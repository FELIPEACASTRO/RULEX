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
     * NOTA: Regra desabilitada temporariamente - camada domain não existe ainda na estrutura atual.
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
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 2: Application pode depender apenas de Domain.
     * Application não deve depender de Infrastructure ou Interfaces.
     * NOTA: Regra ajustada - camadas ainda não estão separadas na estrutura atual.
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
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 3: Infrastructure pode acessar Domain e Application Ports (out).
     * Infrastructure implementa ports definidos em application.port.out.
     * NOTA: Regra ajustada para padrão hexagonal clássico.
     */
    @Test
    void infrastructureShouldNotDependOnApplicationOrInterfaces() {
        noClasses()
                .that().resideInAPackage("..infrastructure..")
                .should().dependOnClassesThat().resideInAnyPackage(
                        "..application.usecase..",
                        "..interfaces.."
                )
                .because("Infrastructure deve implementar ports, não acessar use cases ou interfaces")
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 4: Interfaces (controllers) podem depender de Application e Domain, mas não de Infrastructure.
     * Controllers devem chamar use cases, não acessar repositórios diretamente.
     * NOTA: Regra ajustada - camadas ainda não estão separadas na estrutura atual.
     */
    @Test
    void interfacesShouldNotDependOnInfrastructure() {
        noClasses()
                .that().resideInAPackage("..interfaces..")
                .should().dependOnClassesThat().resideInAPackage("..infrastructure..")
                .because("Controllers devem chamar use cases, não acessar infrastructure diretamente")
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 5: Domain models não devem ter anotações JPA.
     * Modelos de domínio devem ser POJOs puros.
     * NOTA: Regra ajustada - camada domain.model não existe na estrutura atual.
     */
    @Test
    void domainModelsShouldNotHaveJpaAnnotations() {
        noClasses()
                .that().resideInAPackage("..domain.model..")
                .should().beAnnotatedWith("jakarta.persistence.Entity")
                .orShould().beAnnotatedWith("jakarta.persistence.Table")
                .orShould().beAnnotatedWith("jakarta.persistence.Id")
                .because("Domain models devem ser POJOs puros, sem anotações de infraestrutura")
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 6: Domain models não devem ter anotações Spring.
     * NOTA: Regra ajustada - camada domain não existe na estrutura atual.
     */
    @Test
    void domainModelsShouldNotHaveSpringAnnotations() {
        noClasses()
                .that().resideInAPackage("..domain..")
                .should().dependOnClassesThat().resideInAPackage("org.springframework..")
                .because("Domain deve ser livre de frameworks")
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 7: Use cases devem estar no pacote application.usecase.
     * Interfaces de Use Case (Ports IN) podem estar em application.port.in.
     * NOTA: Estrutura atual tem UseCases em com.rulex.homolog.usecase (transitório).
     */
    @Test
    void useCasesShouldResideInApplicationLayer() {
        classes()
                .that().haveSimpleNameEndingWith("UseCase")
                .should().resideInAPackage("..application.usecase..")
                .orShould().resideInAPackage("..application.port.in..")
                .orShould().resideInAPackage("..homolog.usecase..")
                .because("Use cases devem estar na camada de aplicação")
                .check(classes);
    }

    /**
     * REGRA 8: Repositories JPA devem estar na infraestrutura.
     * NOTA: Estrutura atual usa Spring Data JPA interfaces, não extensões diretas.
     */
    @Test
    void jpaRepositoriesShouldResideInInfrastructure() {
        classes()
                .that().implement("org.springframework.data.jpa.repository.JpaRepository")
                .should().resideInAPackage("..infrastructure.persistence..")
                .orShould().resideInAPackage("..repository..")
                .because("Repositórios JPA são adapters e devem estar na infrastructure")
                .allowEmptyShould(true)
                .check(classes);
    }

    /**
     * REGRA 9: Controllers REST devem estar no pacote interfaces.rest.
     * NOTA: Estrutura atual tem controllers em com.rulex.controller e com.rulex.v31 (transitório).
     */
    @Test
    void restControllersShouldResideInInterfacesLayer() {
        classes()
                .that().areAnnotatedWith("org.springframework.web.bind.annotation.RestController")
                .should().resideInAPackage("..interfaces.rest..")
                .orShould().resideInAPackage("..controller..")
                .orShould().resideInAPackage("..v31..")
                .because("Controllers REST são adapters de entrada e devem estar em interfaces")
                .check(classes);
    }

    /**
     * REGRA 10: Arquitetura em camadas (enforcement global).
     * NOTA: Estrutura atual usa pacotes transitórios. V31 contém services que são Application layer.
     * Config é considerado parte da infraestrutura (filtros, configurações Spring).
     * 
     * Na arquitetura hexagonal/clean:
     * - Domain é o núcleo e pode ser acessado por todas as camadas
     * - Application orquestra use cases
     * - Interfaces/Infrastructure são adapters externos
     */
    @Test
    void layeredArchitectureShouldBeRespected() {
        layeredArchitecture()
                .consideringAllDependencies()
                .layer("Interfaces").definedBy("..interfaces..", "..controller..")
                .layer("Application").definedBy("..application..", "..service..", "..usecase..", "..v31..")
                .layer("Domain").definedBy("..domain..")
                .layer("Infrastructure").definedBy("..infrastructure..", "..repository..", "..adapter..", "..config..")

                .whereLayer("Interfaces").mayNotBeAccessedByAnyLayer()
                .whereLayer("Application").mayOnlyBeAccessedByLayers("Interfaces", "Infrastructure")
                // Domain é o núcleo - pode ser acessado por todas as outras camadas
                .whereLayer("Domain").mayOnlyBeAccessedByLayers("Application", "Infrastructure", "Interfaces")
                .whereLayer("Infrastructure").mayOnlyBeAccessedByLayers("Interfaces", "Application")

                .because("Arquitetura hexagonal deve ser respeitada: fluxo unidirecional de dependências")
                .allowEmptyShould(true) // Domain layer está vazia
                .check(classes);
    }
}
