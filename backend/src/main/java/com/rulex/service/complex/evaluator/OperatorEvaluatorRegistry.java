package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.OperatorStatus;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.exception.UnsupportedOperatorException;
import jakarta.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Registry central para avaliadores de operadores.
 * 
 * <p>Mapeia cada ConditionOperator para seu avaliador correspondente.
 * Operadores sem avaliador registrado lançam UnsupportedOperatorException.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class OperatorEvaluatorRegistry {

    private final List<OperatorEvaluator> evaluators;
    private final Map<ConditionOperator, OperatorEvaluator> registry = new EnumMap<>(ConditionOperator.class);
    private final Map<ConditionOperator, OperatorStatus> statusMap = new EnumMap<>(ConditionOperator.class);

    @PostConstruct
    public void init() {
        log.info("Inicializando OperatorEvaluatorRegistry com {} avaliadores", evaluators.size());
        
        for (OperatorEvaluator evaluator : evaluators) {
            String evaluatorName = evaluator.getClass().getSimpleName();
            Set<ConditionOperator> supported = evaluator.getSupportedOperators();
            
            log.debug("Registrando {} com {} operadores", evaluatorName, supported.size());
            
            for (ConditionOperator op : supported) {
                if (registry.containsKey(op)) {
                    log.warn("Operador {} já registrado por {}. Sobrescrevendo com {}",
                        op, registry.get(op).getClass().getSimpleName(), evaluatorName);
                }
                registry.put(op, evaluator);
                
                // Determinar status baseado no tipo de avaliador
                if (evaluator instanceof StubOperatorEvaluator) {
                    statusMap.put(op, OperatorStatus.PLANNED);
                } else {
                    statusMap.put(op, OperatorStatus.STABLE);
                }
            }
        }
        
        // Verificar operadores sem avaliador
        int missing = 0;
        for (ConditionOperator op : ConditionOperator.values()) {
            if (!registry.containsKey(op)) {
                log.error("OPERADOR SEM AVALIADOR: {}. Adicione ao StubOperatorEvaluator ou implemente.", op);
                missing++;
            }
        }
        
        int total = ConditionOperator.values().length;
        int registered = registry.size();
        int stable = (int) statusMap.values().stream().filter(s -> s == OperatorStatus.STABLE).count();
        int planned = (int) statusMap.values().stream().filter(s -> s == OperatorStatus.PLANNED).count();
        
        log.info("OperatorEvaluatorRegistry inicializado:");
        log.info("  - Total de operadores: {}", total);
        log.info("  - Registrados: {}", registered);
        log.info("  - STABLE: {}", stable);
        log.info("  - PLANNED: {}", planned);
        log.info("  - Sem avaliador: {}", missing);
    }

    /**
     * Obtém o avaliador para um operador.
     * @param operator O operador
     * @return O avaliador registrado
     * @throws UnsupportedOperatorException se não há avaliador registrado
     */
    public OperatorEvaluator getEvaluator(ConditionOperator operator) {
        OperatorEvaluator evaluator = registry.get(operator);
        if (evaluator == null) {
            throw new UnsupportedOperatorException(operator, 
                "Nenhum avaliador registrado para este operador.");
        }
        return evaluator;
    }

    /**
     * Obtém o status de um operador.
     */
    public OperatorStatus getStatus(ConditionOperator operator) {
        return statusMap.getOrDefault(operator, OperatorStatus.PLANNED);
    }

    /**
     * Retorna todos os operadores suportados.
     */
    public Set<ConditionOperator> getAllSupportedOperators() {
        return Collections.unmodifiableSet(registry.keySet());
    }

    /**
     * Retorna operadores agrupados por avaliador.
     */
    public Map<String, List<ConditionOperator>> getOperatorsByEvaluator() {
        Map<String, List<ConditionOperator>> result = new HashMap<>();
        for (Map.Entry<ConditionOperator, OperatorEvaluator> entry : registry.entrySet()) {
            String evaluatorName = entry.getValue().getClass().getSimpleName();
            result.computeIfAbsent(evaluatorName, k -> new ArrayList<>()).add(entry.getKey());
        }
        return result;
    }

    /**
     * Retorna operadores agrupados por status.
     */
    public Map<OperatorStatus, List<ConditionOperator>> getOperatorsByStatus() {
        Map<OperatorStatus, List<ConditionOperator>> result = new EnumMap<>(OperatorStatus.class);
        for (Map.Entry<ConditionOperator, OperatorStatus> entry : statusMap.entrySet()) {
            result.computeIfAbsent(entry.getValue(), k -> new ArrayList<>()).add(entry.getKey());
        }
        return result;
    }

    /**
     * Verifica se um operador está implementado (não é PLANNED).
     */
    public boolean isImplemented(ConditionOperator operator) {
        OperatorStatus status = getStatus(operator);
        return status == OperatorStatus.STABLE || status == OperatorStatus.BETA;
    }

    /**
     * Retorna estatísticas do registry.
     */
    public Map<String, Object> getStats() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalOperators", ConditionOperator.values().length);
        stats.put("registeredOperators", registry.size());
        stats.put("evaluatorCount", evaluators.size());
        
        Map<OperatorStatus, Long> byStatus = new EnumMap<>(OperatorStatus.class);
        for (OperatorStatus status : OperatorStatus.values()) {
            byStatus.put(status, statusMap.values().stream().filter(s -> s == status).count());
        }
        stats.put("byStatus", byStatus);
        
        return stats;
    }
}
