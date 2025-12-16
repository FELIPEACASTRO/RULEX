package com.rulex.service;

import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Serviço para gerenciamento de configurações de regras.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RuleConfigurationService {

    private final RuleConfigurationRepository ruleConfigRepository;
    private final AuditService auditService;

    /**
     * Lista todas as regras.
     */
    public Page<RuleConfigurationDTO> listRules(Pageable pageable) {
        Page<RuleConfiguration> rules = ruleConfigRepository.findAll(pageable);
        return rules.map(this::convertToDTO);
    }

    /**
     * Obtém uma regra pelo ID.
     */
    public RuleConfigurationDTO getRuleById(Long id) {
        RuleConfiguration rule = ruleConfigRepository.findById(id)
            .orElseThrow(() -> new RuntimeException("Regra não encontrada"));
        return convertToDTO(rule);
    }

    /**
     * Cria uma nova regra.
     */
    public RuleConfigurationDTO createRule(RuleConfigurationDTO dto) {
        // Verificar se já existe uma regra com este nome
        if (ruleConfigRepository.findByRuleName(dto.getRuleName()).isPresent()) {
            throw new RuntimeException("Já existe uma regra com este nome");
        }

        RuleConfiguration rule = RuleConfiguration.builder()
            .ruleName(dto.getRuleName())
            .description(dto.getDescription())
            .ruleType(RuleConfiguration.RuleType.valueOf(dto.getRuleType()))
            .threshold(dto.getThreshold())
            .weight(dto.getWeight())
            .enabled(dto.getEnabled())
            .classification(TransactionDecision.TransactionClassification.valueOf(dto.getClassification()))
            .parameters(dto.getParameters())
            .build();

        rule = ruleConfigRepository.save(rule);
        
        auditService.logRuleCreated(dto.getRuleName(), "SYSTEM");
        log.info("Regra criada: {}", dto.getRuleName());
        
        return convertToDTO(rule);
    }

    /**
     * Atualiza uma regra existente.
     */
    public RuleConfigurationDTO updateRule(Long id, RuleConfigurationDTO dto) {
        RuleConfiguration rule = ruleConfigRepository.findById(id)
            .orElseThrow(() -> new RuntimeException("Regra não encontrada"));

        rule.setDescription(dto.getDescription());
        rule.setThreshold(dto.getThreshold());
        rule.setWeight(dto.getWeight());
        rule.setEnabled(dto.getEnabled());
        rule.setClassification(TransactionDecision.TransactionClassification.valueOf(dto.getClassification()));
        rule.setParameters(dto.getParameters());
        rule.setVersion(rule.getVersion() + 1);

        rule = ruleConfigRepository.save(rule);
        
        auditService.logRuleUpdated(rule.getRuleName(), 
            java.util.Map.of(
                "threshold", dto.getThreshold(),
                "weight", dto.getWeight(),
                "enabled", dto.getEnabled()
            ), "SYSTEM");
        
        log.info("Regra atualizada: {}", rule.getRuleName());
        
        return convertToDTO(rule);
    }

    /**
     * Deleta uma regra.
     */
    public void deleteRule(Long id) {
        RuleConfiguration rule = ruleConfigRepository.findById(id)
            .orElseThrow(() -> new RuntimeException("Regra não encontrada"));

        ruleConfigRepository.delete(rule);
        
        auditService.logRuleDeleted(rule.getRuleName(), "SYSTEM");
        log.info("Regra deletada: {}", rule.getRuleName());
    }

    /**
     * Ativa/desativa uma regra.
     */
    public RuleConfigurationDTO toggleRule(Long id) {
        RuleConfiguration rule = ruleConfigRepository.findById(id)
            .orElseThrow(() -> new RuntimeException("Regra não encontrada"));

        rule.setEnabled(!rule.getEnabled());
        rule = ruleConfigRepository.save(rule);
        
        auditService.logRuleUpdated(rule.getRuleName(), 
            java.util.Map.of("enabled", rule.getEnabled()), "SYSTEM");
        
        log.info("Regra alternada: {} - Habilitada: {}", rule.getRuleName(), rule.getEnabled());
        
        return convertToDTO(rule);
    }

    /**
     * Lista regras por status de habilitação.
     */
    public List<RuleConfigurationDTO> listRulesByEnabled(Boolean enabled) {
        return ruleConfigRepository.findByEnabled(enabled)
            .stream()
            .map(this::convertToDTO)
            .collect(Collectors.toList());
    }

    /**
     * Converte RuleConfiguration para DTO.
     */
    private RuleConfigurationDTO convertToDTO(RuleConfiguration rule) {
        return RuleConfigurationDTO.builder()
            .id(rule.getId())
            .ruleName(rule.getRuleName())
            .description(rule.getDescription())
            .ruleType(rule.getRuleType().name())
            .threshold(rule.getThreshold())
            .weight(rule.getWeight())
            .enabled(rule.getEnabled())
            .classification(rule.getClassification().name())
            .parameters(rule.getParameters())
            .version(rule.getVersion())
            .build();
    }

}
