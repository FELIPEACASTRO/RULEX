# RES-001: Backup e Restore - RULEX

## Visão Geral

Este documento descreve os procedimentos de backup e restore para o sistema RULEX.

## Componentes que Requerem Backup

| Componente | Tipo | Criticidade | RPO | RTO |
|------------|------|-------------|-----|-----|
| PostgreSQL | Banco de dados principal | CRÍTICO | 1h | 4h |
| Redis | Cache e velocidade | MÉDIO | 24h | 1h |
| Neo4j | Grafo de transações | MÉDIO | 24h | 4h |

## 1. PostgreSQL Backup

### 1.1 Backup Completo (pg_dump)

```bash
#!/bin/bash
# backup-postgres.sh

BACKUP_DIR="/backups/postgres"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
DB_NAME="rulex_db"
DB_USER="${POSTGRES_USER}"
DB_HOST="${POSTGRES_HOST:-localhost}"

# Criar diretório se não existir
mkdir -p ${BACKUP_DIR}

# Backup completo com compressão
pg_dump -h ${DB_HOST} -U ${DB_USER} -d ${DB_NAME} \
  --format=custom \
  --compress=9 \
  --file="${BACKUP_DIR}/rulex_${TIMESTAMP}.dump"

# Verificar integridade
pg_restore --list "${BACKUP_DIR}/rulex_${TIMESTAMP}.dump" > /dev/null 2>&1
if [ $? -eq 0 ]; then
  echo "Backup verificado com sucesso: rulex_${TIMESTAMP}.dump"
else
  echo "ERRO: Backup corrompido!"
  exit 1
fi

# Rotação: manter últimos 7 dias
find ${BACKUP_DIR} -name "rulex_*.dump" -mtime +7 -delete
```

### 1.2 Backup Incremental (WAL Archiving)

Configurar em `postgresql.conf`:

```ini
# WAL Archiving para Point-in-Time Recovery
wal_level = replica
archive_mode = on
archive_command = 'cp %p /backups/postgres/wal/%f'
archive_timeout = 300
```

### 1.3 Restore

```bash
# Restore completo
pg_restore -h ${DB_HOST} -U ${DB_USER} -d rulex_db \
  --clean --if-exists \
  /backups/postgres/rulex_YYYYMMDD_HHMMSS.dump

# Point-in-Time Recovery
pg_restore -h ${DB_HOST} -U ${DB_USER} -d rulex_db \
  --target-time="2025-01-30 14:00:00" \
  /backups/postgres/rulex_base.dump
```

## 2. Redis Backup

### 2.1 RDB Snapshot

```bash
#!/bin/bash
# backup-redis.sh

BACKUP_DIR="/backups/redis"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REDIS_HOST="${REDIS_HOST:-localhost}"

mkdir -p ${BACKUP_DIR}

# Trigger BGSAVE
redis-cli -h ${REDIS_HOST} BGSAVE

# Aguardar conclusão
while [ $(redis-cli -h ${REDIS_HOST} LASTSAVE) == $(redis-cli -h ${REDIS_HOST} LASTSAVE) ]; do
  sleep 1
done

# Copiar dump.rdb
cp /var/lib/redis/dump.rdb "${BACKUP_DIR}/redis_${TIMESTAMP}.rdb"

# Rotação: manter últimos 3 dias
find ${BACKUP_DIR} -name "redis_*.rdb" -mtime +3 -delete
```

### 2.2 AOF (Append Only File)

Configurar em `redis.conf`:

```ini
appendonly yes
appendfsync everysec
auto-aof-rewrite-percentage 100
auto-aof-rewrite-min-size 64mb
```

### 2.3 Restore

```bash
# Parar Redis
systemctl stop redis

# Restaurar RDB
cp /backups/redis/redis_YYYYMMDD_HHMMSS.rdb /var/lib/redis/dump.rdb

# Iniciar Redis
systemctl start redis
```

## 3. Neo4j Backup

### 3.1 Backup Online

```bash
#!/bin/bash
# backup-neo4j.sh

BACKUP_DIR="/backups/neo4j"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
NEO4J_HOME="/var/lib/neo4j"

mkdir -p ${BACKUP_DIR}

# Backup online (requer Enterprise ou usar dump offline)
neo4j-admin database dump neo4j \
  --to-path="${BACKUP_DIR}/neo4j_${TIMESTAMP}.dump"

# Rotação: manter últimos 7 dias
find ${BACKUP_DIR} -name "neo4j_*.dump" -mtime +7 -delete
```

### 3.2 Restore

```bash
# Parar Neo4j
systemctl stop neo4j

# Restore
neo4j-admin database load neo4j \
  --from-path=/backups/neo4j/neo4j_YYYYMMDD_HHMMSS.dump \
  --overwrite-destination=true

# Iniciar Neo4j
systemctl start neo4j
```

## 4. Automação com Cron

```cron
# /etc/cron.d/rulex-backup

# PostgreSQL: backup diário às 2h
0 2 * * * root /opt/rulex/scripts/backup-postgres.sh >> /var/log/rulex-backup.log 2>&1

# Redis: backup a cada 6h
0 */6 * * * root /opt/rulex/scripts/backup-redis.sh >> /var/log/rulex-backup.log 2>&1

# Neo4j: backup diário às 3h
0 3 * * * root /opt/rulex/scripts/backup-neo4j.sh >> /var/log/rulex-backup.log 2>&1
```

## 5. Verificação de Backups

```bash
#!/bin/bash
# verify-backups.sh

echo "=== Verificação de Backups RULEX ==="

# PostgreSQL
LATEST_PG=$(ls -t /backups/postgres/rulex_*.dump 2>/dev/null | head -1)
if [ -n "$LATEST_PG" ]; then
  AGE_HOURS=$(( ($(date +%s) - $(stat -c %Y "$LATEST_PG")) / 3600 ))
  echo "PostgreSQL: $LATEST_PG (${AGE_HOURS}h atrás)"
  [ $AGE_HOURS -gt 24 ] && echo "  ⚠️ ALERTA: Backup com mais de 24h!"
else
  echo "PostgreSQL: ❌ NENHUM BACKUP ENCONTRADO!"
fi

# Redis
LATEST_REDIS=$(ls -t /backups/redis/redis_*.rdb 2>/dev/null | head -1)
if [ -n "$LATEST_REDIS" ]; then
  AGE_HOURS=$(( ($(date +%s) - $(stat -c %Y "$LATEST_REDIS")) / 3600 ))
  echo "Redis: $LATEST_REDIS (${AGE_HOURS}h atrás)"
else
  echo "Redis: ❌ NENHUM BACKUP ENCONTRADO!"
fi

# Neo4j
LATEST_NEO4J=$(ls -t /backups/neo4j/neo4j_*.dump 2>/dev/null | head -1)
if [ -n "$LATEST_NEO4J" ]; then
  AGE_HOURS=$(( ($(date +%s) - $(stat -c %Y "$LATEST_NEO4J")) / 3600 ))
  echo "Neo4j: $LATEST_NEO4J (${AGE_HOURS}h atrás)"
else
  echo "Neo4j: ❌ NENHUM BACKUP ENCONTRADO!"
fi
```

## 6. Disaster Recovery

### 6.1 Procedimento de Recovery Completo

1. **Provisionar infraestrutura** (PostgreSQL, Redis, Neo4j)
2. **Restaurar PostgreSQL** (dados críticos)
3. **Restaurar Neo4j** (grafo de transações)
4. **Redis** pode ser reconstruído do PostgreSQL se necessário
5. **Validar** integridade dos dados
6. **Iniciar aplicação** RULEX

### 6.2 Checklist de Validação

- [ ] PostgreSQL respondendo queries
- [ ] Flyway migrations aplicadas
- [ ] Redis conectado e respondendo PING
- [ ] Neo4j conectado e grafo acessível
- [ ] Health check `/actuator/health` retornando UP
- [ ] Teste de transação de exemplo

## 7. Monitoramento de Backups

Métricas a monitorar:

| Métrica | Alerta |
|---------|--------|
| `backup_age_hours` | > 24h |
| `backup_size_bytes` | Variação > 50% |
| `backup_duration_seconds` | > 1h |
| `backup_success` | = 0 |

## 8. Contatos de Emergência

| Função | Contato |
|--------|---------|
| DBA | dba@empresa.com |
| DevOps | devops@empresa.com |
| Suporte RULEX | support@cognition.ai |
