import axios from 'axios';

const api = axios.create({
  baseURL: '/api',
  headers: {
    'Content-Type': 'application/json',
  },
});

export interface Rule {
  id: number;
  name: string;
  description?: string;
  conditions: any;
  weight: number;
  classification: string;
  category: string;
  enabled?: boolean;
  isActive?: boolean;
  version?: number;
  createdAt?: string;
  updatedAt?: string;
  logicOperator?: string;
  source?: string;
}

export interface CreateRuleDto {
  name: string;
  description?: string;
  conditions: string;
  weight: number;
  classification: string;
  category: string;
  enabled?: boolean;
}

export interface UpdateRuleDto extends Partial<CreateRuleDto> {
  id: number;
}

export const rulesApi = {
  list: async (): Promise<Rule[]> => {
    const response = await api.get('/rules');
    const data = response.data;
    if (Array.isArray(data)) {
      return data;
    }
    if (data && Array.isArray(data.content)) {
      return data.content;
    }
    return [];
  },

  getById: async (id: number): Promise<Rule> => {
    const response = await api.get(`/rules/${id}`);
    return response.data;
  },

  create: async (rule: CreateRuleDto): Promise<Rule> => {
    const response = await api.post('/rules', rule);
    return response.data;
  },

  update: async (rule: UpdateRuleDto): Promise<Rule> => {
    const response = await api.put(`/rules/${rule.id}`, rule);
    return response.data;
  },

  delete: async (id: number): Promise<void> => {
    await api.delete(`/rules/${id}`);
  },

  toggle: async (id: number, enabled: boolean): Promise<Rule> => {
    const response = await api.patch(`/rules/${id}/toggle`, { enabled });
    return response.data;
  },
};

export default api;
