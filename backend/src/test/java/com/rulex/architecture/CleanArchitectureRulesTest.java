package com.rulex.architecture;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import org.junit.jupiter.api.Test;

public class CleanArchitectureRulesTest {

  private static final String BASE_PACKAGE = "com.rulex";

  @Test
  void useCasesAndPortsMustNotDependOnSpringOrJackson() {
    JavaClasses classes =
        new ClassFileImporter()
            .withImportOption(new ImportOption.DoNotIncludeTests())
            .importPackages(BASE_PACKAGE);

    noClasses()
        .that()
        .resideInAnyPackage("..homolog.port..")
        .or()
        .resideInAnyPackage("..homolog.usecase..")
        .and()
        .haveSimpleNameEndingWith("UseCase")
        .should()
        .dependOnClassesThat()
        .resideInAnyPackage(
            "org.springframework..",
            "jakarta.persistence..",
            "jakarta.transaction..",
            "com.fasterxml.jackson..",
            "com.rulex.repository..")
        .check(classes);
  }
}
