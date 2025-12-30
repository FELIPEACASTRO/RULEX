package com.rulex.dto.complex;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para grupos de condições com suporte a aninhamento.
 * Permite criar estruturas como: (A AND B) OR (C AND D)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ConditionGroupDTO {

    private UUID id;

    @NotNull(message = "Operador lógico é obrigatório")
    private LogicOperatorType logicOperator;

    private String name;
    
    private String description;

    private Integer position;

    private Boolean enabled;

    /**
     * Condições diretas deste grupo
     */
    @Valid
    private List<ConditionDTO> conditions;

    /**
     * Grupos filhos (aninhados) - permite criar estruturas complexas
     */
    @Valid
    private List<ConditionGroupDTO> children;

    /**
     * Operadores lógicos suportados
     */
    public enum LogicOperatorType {
        AND,    // Todas as condições devem ser verdadeiras
        OR,     // Pelo menos uma condição deve ser verdadeira
        NOT,    // Inverte o resultado do grupo
        XOR,    // Exatamente uma condição deve ser verdadeira
        NAND,   // NOT AND - pelo menos uma condição deve ser falsa
        NOR     // NOT OR - todas as condições devem ser falsas
    }
}
