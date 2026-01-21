# Backend (Spring Boot Java 21)
# INFRA-002 FIX: Multi-stage build otimizado com imagem Alpine e usuário não-root

# =============================================================================
# Stage 1: Build
# =============================================================================
FROM maven:3.9.9-eclipse-temurin-21 AS build
WORKDIR /app

# Cache de dependências (layer separado para melhor cache)
COPY pom.xml ./
RUN mvn dependency:go-offline -B

# Build do projeto
COPY src ./src
RUN mvn -q -DskipTests package -B

# =============================================================================
# Stage 2: Runtime (imagem mínima)
# =============================================================================
FROM eclipse-temurin:21-jre-alpine
WORKDIR /app

# Criar usuário não-root para segurança
RUN addgroup -S rulex && adduser -S rulex -G rulex

# Instalar curl para healthcheck
RUN apk add --no-cache curl

# Copiar apenas o JAR necessário
COPY --from=build /app/target/*.jar /app/app.jar

# Mudar para usuário não-root
USER rulex

# Variáveis de ambiente
ENV JAVA_OPTS="-XX:+UseContainerSupport -XX:MaxRAMPercentage=75.0 -XX:+UseG1GC"

# Healthcheck
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8080/api/actuator/health/liveness || exit 1

# Expor porta
EXPOSE 8080

# Entrypoint
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /app/app.jar"]
