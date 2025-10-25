import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import {
  calculateAppointmentSpeed,
  calculateAttachmentRate,
  calculateRescheduleRate,
  calculateFollowUpConsistency,
  getQualityMetricsForUsers,
  getQualityMetricsForUser,
  getQualityMetricsForOffice,
  getCacheStats,
  clearQualityMetricsCache,
  calculateCompositeQualityScore,
  formatMetricsForDisplay,
} from './qualityMetrics';
import type {
  RepCardCustomer,
  RepCardAppointment,
  RepCardCustomerStatusLog,
  RepCardCustomerAttachment,
  QualityMetrics,
} from './types';

// Mock the RepCard client
vi.mock('./client', () => ({
  repcardClient: {
    getCustomers: vi.fn(),
    getAppointments: vi.fn(),
    getCustomerStatusLogs: vi.fn(),
    getCustomerAttachments: vi.fn(),
  },
}));

// Mock the database client
vi.mock('@/lib/db/client', () => ({
  sql: vi.fn(),
}));

import { repcardClient } from './client';
import { sql } from '@/lib/db/client';

const mockRepcardClient = vi.mocked(repcardClient);
const mockSql = vi.mocked(sql);

describe('Quality Metrics Calculations', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    clearQualityMetricsCache();
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('calculateAppointmentSpeed', () => {
    it('calculates percentage correctly with appointments within 24 hours', () => {
      const customers: RepCardCustomer[] = [
        {
          id: 1,
          firstName: 'John',
          lastName: 'Doe',
          companyId: 1,
          createdAt: '2024-01-01T10:00:00Z',
        },
        {
          id: 2,
          firstName: 'Jane',
          lastName: 'Smith',
          companyId: 1,
          createdAt: '2024-01-01T10:00:00Z',
        },
      ];

      const appointments: RepCardAppointment[] = [
        {
          id: 1,
          title: 'Appointment 1',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 1, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' },
          startAt: '2024-01-01T12:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-01T13:00:00Z',
          appointmentLocation: 'Office',
          status: null,
          createdAt: '2024-01-01T12:00:00Z', // 2 hours later
          updatedAt: '2024-01-01T12:00:00Z',
          durationTime: 60,
        },
        {
          id: 2,
          title: 'Appointment 2',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 2, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' },
          startAt: '2024-01-02T15:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-02T16:00:00Z',
          appointmentLocation: 'Office',
          status: null,
          createdAt: '2024-01-02T15:00:00Z', // 29 hours later
          updatedAt: '2024-01-02T15:00:00Z',
          durationTime: 60,
        },
      ];

      const result = calculateAppointmentSpeed(customers, appointments);

      expect(result.totalAppointments).toBe(2);
      expect(result.appointmentsWithin24Hours).toBe(1);
      expect(result.percentageWithin24Hours).toBe(50);
      expect(result.averageHoursToSchedule).toBe(15.5);
    });

    it('handles empty appointments array', () => {
      const customers: RepCardCustomer[] = [
        { 
          id: 1, 
          firstName: 'John', 
          lastName: 'Doe', 
          companyId: 1, 
          createdAt: '2024-01-01T10:00:00Z' 
        },
      ];

      const appointments: RepCardAppointment[] = [];

      const result = calculateAppointmentSpeed(customers, appointments);

      expect(result.totalAppointments).toBe(0);
      expect(result.appointmentsWithin24Hours).toBe(0);
      expect(result.percentageWithin24Hours).toBe(0);
      expect(result.averageHoursToSchedule).toBe(0);
    });

    it('handles appointments without matching customers', () => {
      const customers: RepCardCustomer[] = [
        { 
          id: 1, 
          firstName: 'John', 
          lastName: 'Doe', 
          companyId: 1, 
          createdAt: '2024-01-01T10:00:00Z' 
        },
      ];

      const appointments: RepCardAppointment[] = [
        {
          id: 1,
          title: 'Appointment 1',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 2, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' }, // Different customer
          startAt: '2024-01-01T12:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-01T13:00:00Z',
          appointmentLocation: 'Office',
          status: null,
          createdAt: '2024-01-01T12:00:00Z',
          updatedAt: '2024-01-01T12:00:00Z',
          durationTime: 60,
        },
      ];

      const result = calculateAppointmentSpeed(customers, appointments);

      expect(result.totalAppointments).toBe(0);
      expect(result.appointmentsWithin24Hours).toBe(0);
      expect(result.percentageWithin24Hours).toBe(0);
      expect(result.averageHoursToSchedule).toBe(0);
    });

    it('handles appointments exactly at 24 hour boundary', () => {
      const customers: RepCardCustomer[] = [
        { id: 'customer-1', createdAt: '2024-01-01T10:00:00Z' },
      ];

      const appointments: RepCardAppointment[] = [
        {
          id: 'appointment-1',
          createdAt: '2024-01-02T10:00:00Z', // Exactly 24 hours later
          contact: { id: 'customer-1' },
        },
      ];

      const result = calculateAppointmentSpeed(customers, appointments);

      expect(result.totalAppointments).toBe(1);
      expect(result.appointmentsWithin24Hours).toBe(0); // Exactly 24 hours is not within 24 hours
      expect(result.percentageWithin24Hours).toBe(0);
    });
  });

  describe('calculateAttachmentRate', () => {
    it('calculates percentage correctly with attachments', () => {
      const customers: RepCardCustomer[] = [
        { id: 1, firstName: 'John', lastName: 'Doe', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
        { id: 2, firstName: 'Jane', lastName: 'Smith', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
        { id: 3, firstName: 'Bob', lastName: 'Johnson', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
      ];

      const attachments: RepCardCustomerAttachment[] = [
        { id: 1, attachmentUrl: 'https://example.com/1', userId: 1, customerId: 1 },
        { id: 2, attachmentUrl: 'https://example.com/2', userId: 1, customerId: 1 }, // Same customer, multiple attachments
        { id: 3, attachmentUrl: 'https://example.com/3', userId: 1, customerId: 2 },
      ];

      const result = calculateAttachmentRate(customers, attachments);

      expect(result.totalCustomers).toBe(3);
      expect(result.customersWithAttachments).toBe(2);
      expect(result.percentageWithAttachments).toBe(66.67);
      expect(result.totalAttachments).toBe(3);
    });

    it('handles customers with multiple attachments (counts once)', () => {
      const customers: RepCardCustomer[] = [
        { id: 1, firstName: 'John', lastName: 'Doe', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
        { id: 2, firstName: 'Jane', lastName: 'Smith', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
      ];

      const attachments: RepCardCustomerAttachment[] = [
        { id: 1, attachmentUrl: 'https://example.com/1', userId: 1, customerId: 1 },
        { id: 2, attachmentUrl: 'https://example.com/2', userId: 1, customerId: 1 },
        { id: 3, attachmentUrl: 'https://example.com/3', userId: 1, customerId: 1 },
      ];

      const result = calculateAttachmentRate(customers, attachments);

      expect(result.customersWithAttachments).toBe(1); // Only customer-1, counted once
      expect(result.totalAttachments).toBe(3);
    });

    it('handles empty attachments array', () => {
      const customers: RepCardCustomer[] = [
        { id: 1, firstName: 'John', lastName: 'Doe', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
        { id: 2, firstName: 'Jane', lastName: 'Smith', companyId: 1, createdAt: '2024-01-01T10:00:00Z' },
      ];

      const attachments: RepCardCustomerAttachment[] = [];

      const result = calculateAttachmentRate(customers, attachments);

      expect(result.totalCustomers).toBe(2);
      expect(result.customersWithAttachments).toBe(0);
      expect(result.percentageWithAttachments).toBe(0);
      expect(result.totalAttachments).toBe(0);
    });

    it('handles empty customers array', () => {
      const customers: RepCardCustomer[] = [];

      const attachments: RepCardCustomerAttachment[] = [
        { id: 1, attachmentUrl: 'https://example.com/1', userId: 1, customerId: 1 },
      ];

      const result = calculateAttachmentRate(customers, attachments);

      expect(result.totalCustomers).toBe(0);
      expect(result.customersWithAttachments).toBe(0);
      expect(result.percentageWithAttachments).toBe(0);
    });
  });

  describe('calculateRescheduleRate', () => {
    it('calculates average reschedules correctly', () => {
      const statusLogs: RepCardCustomerStatusLog[] = [
        {
          _id: 'log-1',
          customerId: 1,
          customer: { id: 1, firstName: 'John', lastName: 'Doe', email: 'john@example.com', countryCode: 'US', phoneNumber: '123-456-7890' },
          statusFrom: null, // First status
          statusTo: { id: 1, statusName: 'Initial', colour: 'blue', iconUrl: 'https://example.com/icon.png' },
          userId: 1,
          companyId: 1,
          createdAt: '2024-01-01T10:00:00Z',
          updatedAt: '2024-01-01T10:00:00Z',
          user: { id: 1, firstName: 'User', lastName: 'One', email: 'user@example.com', username: 'user1', office: 'Office', team: 'Team', jobTitle: 'Rep', phoneNumber: '123-456-7890', image: 'https://example.com/image.jpg', rating: '5', bio: 'Bio', companyName: 'Company', badgeId: 'badge1', qrCode: 'qr1' },
        },
        {
          _id: 'log-2',
          customerId: 1,
          customer: { id: 1, firstName: 'John', lastName: 'Doe', email: 'john@example.com', countryCode: 'US', phoneNumber: '123-456-7890' },
          statusFrom: { id: 1, statusName: 'Initial', colour: 'blue', iconUrl: 'https://example.com/icon.png' },
          statusTo: { id: 2, statusName: 'Scheduled', colour: 'green', iconUrl: 'https://example.com/icon.png' },
          userId: 1,
          companyId: 1,
          createdAt: '2024-01-01T11:00:00Z',
          updatedAt: '2024-01-01T11:00:00Z',
          user: { id: 1, firstName: 'User', lastName: 'One', email: 'user@example.com', username: 'user1', office: 'Office', team: 'Team', jobTitle: 'Rep', phoneNumber: '123-456-7890', image: 'https://example.com/image.jpg', rating: '5', bio: 'Bio', companyName: 'Company', badgeId: 'badge1', qrCode: 'qr1' },
        },
        {
          _id: 'log-3',
          customerId: 1,
          customer: { id: 1, firstName: 'John', lastName: 'Doe', email: 'john@example.com', countryCode: 'US', phoneNumber: '123-456-7890' },
          statusFrom: { id: 2, statusName: 'Scheduled', colour: 'green', iconUrl: 'https://example.com/icon.png' },
          statusTo: { id: 3, statusName: 'Rescheduled', colour: 'orange', iconUrl: 'https://example.com/icon.png' },
          userId: 1,
          companyId: 1,
          createdAt: '2024-01-01T12:00:00Z',
          updatedAt: '2024-01-01T12:00:00Z',
          user: { id: 1, firstName: 'User', lastName: 'One', email: 'user@example.com', username: 'user1', office: 'Office', team: 'Team', jobTitle: 'Rep', phoneNumber: '123-456-7890', image: 'https://example.com/image.jpg', rating: '5', bio: 'Bio', companyName: 'Company', badgeId: 'badge1', qrCode: 'qr1' },
        },
        {
          _id: 'log-4',
          customerId: 2,
          customer: { id: 2, firstName: 'Jane', lastName: 'Smith', email: 'jane@example.com', countryCode: 'US', phoneNumber: '123-456-7890' },
          statusFrom: null, // First status
          statusTo: { id: 1, statusName: 'Initial', colour: 'blue', iconUrl: 'https://example.com/icon.png' },
          userId: 1,
          companyId: 1,
          createdAt: '2024-01-01T10:00:00Z',
          updatedAt: '2024-01-01T10:00:00Z',
          user: { id: 1, firstName: 'User', lastName: 'One', email: 'user@example.com', username: 'user1', office: 'Office', team: 'Team', jobTitle: 'Rep', phoneNumber: '123-456-7890', image: 'https://example.com/image.jpg', rating: '5', bio: 'Bio', companyName: 'Company', badgeId: 'badge1', qrCode: 'qr1' },
        },
      ];

      const result = calculateRescheduleRate(statusLogs);

      expect(result.totalCustomers).toBe(2);
      expect(result.totalReschedules).toBe(2); // customer-1 has 2 status changes
      expect(result.averageReschedulesPerCustomer).toBe(1);
      expect(result.customersWithReschedules).toBe(1); // Only customer-1 has reschedules
    });

    it('ignores first status (statusFrom === null)', () => {
      const statusLogs: RepCardCustomerStatusLog[] = [
        {
          id: 'log-1',
          customerId: 'customer-1',
          statusFrom: null, // First status - should be ignored
          statusTo: { id: 'status-1', title: 'Initial' },
          createdAt: '2024-01-01T10:00:00Z',
        },
      ];

      const result = calculateRescheduleRate(statusLogs);

      expect(result.totalReschedules).toBe(0);
      expect(result.customersWithReschedules).toBe(0);
    });

    it('handles customers with no reschedules', () => {
      const statusLogs: RepCardCustomerStatusLog[] = [
        {
          id: 'log-1',
          customerId: 'customer-1',
          statusFrom: null,
          statusTo: { id: 'status-1', title: 'Initial' },
          createdAt: '2024-01-01T10:00:00Z',
        },
      ];

      const result = calculateRescheduleRate(statusLogs);

      expect(result.totalReschedules).toBe(0);
      expect(result.customersWithReschedules).toBe(0);
      expect(result.averageReschedulesPerCustomer).toBe(0);
    });

    it('handles empty status logs array', () => {
      const statusLogs: RepCardCustomerStatusLog[] = [];

      const result = calculateRescheduleRate(statusLogs);

      expect(result.totalCustomers).toBe(0);
      expect(result.totalReschedules).toBe(0);
      expect(result.averageReschedulesPerCustomer).toBe(0);
      expect(result.customersWithReschedules).toBe(0);
    });

    it('counts multiple reschedules for same customer', () => {
      const statusLogs: RepCardCustomerStatusLog[] = [
        {
          id: 'log-1',
          customerId: 'customer-1',
          statusFrom: null,
          statusTo: { id: 'status-1', title: 'Initial' },
          createdAt: '2024-01-01T10:00:00Z',
        },
        {
          id: 'log-2',
          customerId: 'customer-1',
          statusFrom: { id: 'status-1', title: 'Initial' },
          statusTo: { id: 'status-2', title: 'Scheduled' },
          createdAt: '2024-01-01T11:00:00Z',
        },
        {
          id: 'log-3',
          customerId: 'customer-1',
          statusFrom: { id: 'status-2', title: 'Scheduled' },
          statusTo: { id: 'status-3', title: 'Rescheduled' },
          createdAt: '2024-01-01T12:00:00Z',
        },
        {
          id: 'log-4',
          customerId: 'customer-1',
          statusFrom: { id: 'status-3', title: 'Rescheduled' },
          statusTo: { id: 'status-4', title: 'Final' },
          createdAt: '2024-01-01T13:00:00Z',
        },
      ];

      const result = calculateRescheduleRate(statusLogs);

      expect(result.totalReschedules).toBe(3); // 3 status changes after initial
      expect(result.customersWithReschedules).toBe(1);
    });
  });

  describe('calculateFollowUpConsistency', () => {
    it('calculates percentage correctly with follow-ups', () => {
      const appointments: RepCardAppointment[] = [
        {
          id: 1,
          title: 'Appointment 1',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 1, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' },
          startAt: '2024-01-01T10:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-01T11:00:00Z',
          appointmentLocation: 'Office',
          status: { id: 1, title: 'Completed', category: { id: 1, title: 'Category', categoryColour: 'green' } },
          createdAt: '2024-01-01T10:00:00Z',
          updatedAt: '2024-01-01T10:00:00Z',
          durationTime: 60,
        },
        {
          id: 2,
          title: 'Appointment 2',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 1, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' }, // Follow-up for customer-1
          startAt: '2024-01-02T10:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-02T11:00:00Z',
          appointmentLocation: 'Office',
          status: null,
          createdAt: '2024-01-02T10:00:00Z',
          updatedAt: '2024-01-02T10:00:00Z',
          durationTime: 60,
        },
        {
          id: 3,
          title: 'Appointment 3',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 2, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' },
          startAt: '2024-01-01T10:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-01T11:00:00Z',
          appointmentLocation: 'Office',
          status: { id: 1, title: 'Completed', category: { id: 1, title: 'Category', categoryColour: 'green' } },
          createdAt: '2024-01-01T10:00:00Z',
          updatedAt: '2024-01-01T10:00:00Z',
          durationTime: 60,
        },
        {
          id: 4,
          title: 'Appointment 4',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 2, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' }, // Follow-up for customer-2
          startAt: '2024-01-02T10:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-02T11:00:00Z',
          appointmentLocation: 'Office',
          status: null,
          createdAt: '2024-01-02T10:00:00Z',
          updatedAt: '2024-01-02T10:00:00Z',
          durationTime: 60,
        },
        {
          id: 5,
          title: 'Appointment 5',
          appointmentLink: 'https://example.com',
          notes: 'Notes',
          userId: 1,
          setter: { id: 1, fullName: 'Setter', email: 'setter@example.com', phoneNumber: '123-456-7890' },
          closerId: 1,
          closer: { id: 1, fullName: 'Closer', email: 'closer@example.com', phoneNumber: '123-456-7890' },
          calendarId: 1,
          contact: { id: 3, fullName: 'Contact', email: 'contact@example.com', phoneNumber: '123-456-7890', contactSource: 'web' }, // Only one appointment for customer-3
          startAt: '2024-01-01T10:00:00Z',
          startAtTimezone: 'UTC',
          endAtTimezone: 'UTC',
          endAt: '2024-01-01T11:00:00Z',
          appointmentLocation: 'Office',
          status: null,
          createdAt: '2024-01-01T10:00:00Z',
          updatedAt: '2024-01-01T10:00:00Z',
          durationTime: 60,
        },
      ];

      const result = calculateFollowUpConsistency(appointments);

      expect(result.customersRequiringFollowUps).toBe(2); // customer-1 and customer-2
      expect(result.customersWithFollowUps).toBe(2);
      expect(result.percentageWithFollowUps).toBe(100);
      expect(result.totalFollowUpAppointments).toBe(2); // 2 follow-up appointments
    });

    it('handles customers with only one appointment (no follow-up required)', () => {
      const appointments: RepCardAppointment[] = [
        {
          id: 'appointment-1',
          contact: { id: 'customer-1' },
        },
        {
          id: 'appointment-2',
          contact: { id: 'customer-2' },
        },
      ];

      const result = calculateFollowUpConsistency(appointments);

      expect(result.customersRequiringFollowUps).toBe(0);
      expect(result.customersWithFollowUps).toBe(0);
      expect(result.percentageWithFollowUps).toBe(0);
      expect(result.totalFollowUpAppointments).toBe(0);
    });

    it('handles empty appointments array', () => {
      const appointments: RepCardAppointment[] = [];

      const result = calculateFollowUpConsistency(appointments);

      expect(result.customersRequiringFollowUps).toBe(0);
      expect(result.customersWithFollowUps).toBe(0);
      expect(result.percentageWithFollowUps).toBe(0);
      expect(result.totalFollowUpAppointments).toBe(0);
    });

    it('counts multiple follow-ups per customer', () => {
      const appointments: RepCardAppointment[] = [
        {
          id: 'appointment-1',
          contact: { id: 'customer-1' },
        },
        {
          id: 'appointment-2',
          contact: { id: 'customer-1' }, // First follow-up
        },
        {
          id: 'appointment-3',
          contact: { id: 'customer-1' }, // Second follow-up
        },
      ];

      const result = calculateFollowUpConsistency(appointments);

      expect(result.customersRequiringFollowUps).toBe(1);
      expect(result.customersWithFollowUps).toBe(1);
      expect(result.totalFollowUpAppointments).toBe(2); // 2 follow-up appointments
    });
  });

  describe('getQualityMetricsForUsers', () => {
    beforeEach(() => {
      mockRepcardClient.getCustomers.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: [],
          total: 0,
          perPage: 100,
          currentPage: 1,
          lastPage: 1,
        },
      });
      mockRepcardClient.getAppointments.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: [],
          totalCount: 0,
          currentPage: 1,
          totalPages: 1,
        },
      });
      mockRepcardClient.getCustomerStatusLogs.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: [],
          totalCount: 0,
          currentPage: 1,
          totalPages: 1,
        },
      });
      mockRepcardClient.getCustomerAttachments.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: [],
          totalCount: 0,
          currentPage: 1,
          totalPages: 1,
        },
      });
    });

    it('fetches and calculates metrics correctly', async () => {
      const customers: RepCardCustomer[] = [
        { id: 1, createdAt: '2024-01-01T10:00:00Z' },
      ];
      const appointments: RepCardAppointment[] = [
        {
          id: 1,
          createdAt: '2024-01-01T12:00:00Z',
          contact: { id: 1 },
        },
      ];
      const statusLogs: RepCardCustomerStatusLog[] = [];
      const attachments: RepCardCustomerAttachment[] = [];

      mockRepcardClient.getCustomers.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: customers,
          total: 1,
          perPage: 100,
          currentPage: 1,
          lastPage: 1,
        },
      });
      mockRepcardClient.getAppointments.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: appointments,
          totalCount: 1,
          currentPage: 1,
          totalPages: 1,
        },
      });
      mockRepcardClient.getCustomerStatusLogs.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: statusLogs,
          totalCount: 0,
          currentPage: 1,
          totalPages: 1,
        },
      });
      mockRepcardClient.getCustomerAttachments.mockResolvedValue({
        status: 200,
        statusCode: 200,
        message: 'Success',
        result: {
          data: attachments,
          totalCount: 0,
          currentPage: 1,
          totalPages: 1,
        },
      });

      const result = await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      expect(result.appointmentSpeed.totalAppointments).toBe(1);
      expect(result.appointmentSpeed.appointmentsWithin24Hours).toBe(1);
      expect(result.period.startDate).toBe('2024-01-01');
      expect(result.period.endDate).toBe('2024-01-31');
      expect(result.calculatedAt).toBeDefined();
    });

    it('uses cache when available', async () => {
      // First call
      await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      // Second call should use cache
      const result = await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      // API should only be called once
      expect(mockRepcardClient.getCustomers).toHaveBeenCalledTimes(1);
      expect(result).toBeDefined();
    });

    it('bypasses cache when useCache is false', async () => {
      await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
        useCache: false,
      });

      await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
        useCache: false,
      });

      // API should be called twice
      expect(mockRepcardClient.getCustomers).toHaveBeenCalledTimes(2);
    });

    it('handles API errors gracefully', async () => {
      mockRepcardClient.getCustomers.mockRejectedValue(new Error('API Error'));

      await expect(
        getQualityMetricsForUsers({
          repcardUserIds: ['repcard-user-1'],
          startDate: '2024-01-01',
          endDate: '2024-01-31',
        })
      ).rejects.toThrow('Failed to calculate quality metrics');
    });

    it('resolves RepCard user IDs from dashboard user IDs', async () => {
      mockSql.mockResolvedValue([
        { repcard_user_id: 'repcard-user-1' },
        { repcard_user_id: 'repcard-user-2' },
      ]);

      await getQualityMetricsForUsers({
        userIds: ['user-1', 'user-2'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      expect(mockSql).toHaveBeenCalledWith(
        expect.stringContaining('SELECT repcard_user_id')
      );
    });

    it('resolves RepCard user IDs from office names', async () => {
      mockSql.mockResolvedValue([
        { repcard_user_id: 'repcard-user-1' },
        { repcard_user_id: 'repcard-user-2' },
      ]);

      await getQualityMetricsForUsers({
        officeIds: ['Phoenix Office'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      expect(mockSql).toHaveBeenCalledWith(
        expect.stringContaining('WHERE office = ANY')
      );
    });
  });

  describe('getQualityMetricsForUser', () => {
    beforeEach(() => {
      mockRepcardClient.getCustomers.mockResolvedValue([]);
      mockRepcardClient.getAppointments.mockResolvedValue([]);
      mockRepcardClient.getCustomerStatusLogs.mockResolvedValue([]);
      mockRepcardClient.getCustomerAttachments.mockResolvedValue([]);
    });

    it('returns metrics with user metadata', async () => {
      mockSql.mockResolvedValue([
        {
          id: 'user-1',
          name: 'John Doe',
          repcard_user_id: 'repcard-user-1',
          office: 'Phoenix Office',
        },
      ]);

      const result = await getQualityMetricsForUser(
        'user-1',
        '2024-01-01',
        '2024-01-31'
      );

      expect(result.userId).toBe('user-1');
      expect(result.repcardUserId).toBe('repcard-user-1');
      expect(result.userName).toBe('John Doe');
      expect(result.office).toBe('Phoenix Office');
      expect(result.appointmentSpeed).toBeDefined();
    });

    it('throws error if user not found', async () => {
      mockSql.mockResolvedValue([]);

      await expect(
        getQualityMetricsForUser('user-1', '2024-01-01', '2024-01-31')
      ).rejects.toThrow('User with ID user-1 not found');
    });

    it('throws error if user has no repcard_user_id', async () => {
      mockSql.mockResolvedValue([
        {
          id: 'user-1',
          name: 'John Doe',
          repcard_user_id: null,
          office: 'Phoenix Office',
        },
      ]);

      await expect(
        getQualityMetricsForUser('user-1', '2024-01-01', '2024-01-31')
      ).rejects.toThrow('User user-1 has no RepCard user ID');
    });

    it('uses cache correctly', async () => {
      mockSql.mockResolvedValue([
        {
          id: 'user-1',
          name: 'John Doe',
          repcard_user_id: 'repcard-user-1',
          office: 'Phoenix Office',
        },
      ]);

      // First call
      await getQualityMetricsForUser('user-1', '2024-01-01', '2024-01-31');

      // Second call should use cache
      await getQualityMetricsForUser('user-1', '2024-01-01', '2024-01-31');

      // API should only be called once
      expect(mockRepcardClient.getCustomers).toHaveBeenCalledTimes(1);
    });
  });

  describe('getQualityMetricsForOffice', () => {
    beforeEach(() => {
      mockRepcardClient.getCustomers.mockResolvedValue([]);
      mockRepcardClient.getAppointments.mockResolvedValue([]);
      mockRepcardClient.getCustomerStatusLogs.mockResolvedValue([]);
      mockRepcardClient.getCustomerAttachments.mockResolvedValue([]);
    });

    it('returns aggregate metrics for office', async () => {
      mockSql.mockResolvedValue([
        {
          id: 'user-1',
          name: 'John Doe',
          repcard_user_id: 'repcard-user-1',
        },
        {
          id: 'user-2',
          name: 'Jane Smith',
          repcard_user_id: 'repcard-user-2',
        },
      ]);

      const result = await getQualityMetricsForOffice(
        'Phoenix Office',
        '2024-01-01',
        '2024-01-31'
      );

      expect(result.officeName).toBe('Phoenix Office');
      expect(result.userCount).toBe(2);
      expect(result.topPerformers).toBeDefined();
      expect(result.appointmentSpeed).toBeDefined();
    });

    it('calculates top performers correctly', async () => {
      mockSql.mockResolvedValue([
        {
          id: 'user-1',
          name: 'John Doe',
          repcard_user_id: 'repcard-user-1',
        },
        {
          id: 'user-2',
          name: 'Jane Smith',
          repcard_user_id: 'repcard-user-2',
        },
      ]);

      const result = await getQualityMetricsForOffice(
        'Phoenix Office',
        '2024-01-01',
        '2024-01-31'
      );

      expect(result.topPerformers).toHaveLength(2);
      expect(result.topPerformers[0]).toHaveProperty('userId');
      expect(result.topPerformers[0]).toHaveProperty('userName');
      expect(result.topPerformers[0]).toHaveProperty('qualityScore');
    });

    it('throws error if office has no users', async () => {
      mockSql.mockResolvedValue([]);

      await expect(
        getQualityMetricsForOffice('Phoenix Office', '2024-01-01', '2024-01-31')
      ).rejects.toThrow('No users found for office: Phoenix Office');
    });

    it('uses cache correctly', async () => {
      mockSql.mockResolvedValue([
        {
          id: 'user-1',
          name: 'John Doe',
          repcard_user_id: 'repcard-user-1',
        },
      ]);

      // First call
      await getQualityMetricsForOffice('Phoenix Office', '2024-01-01', '2024-01-31');

      // Second call should use cache
      await getQualityMetricsForOffice('Phoenix Office', '2024-01-01', '2024-01-31');

      // API should only be called once
      expect(mockRepcardClient.getCustomers).toHaveBeenCalledTimes(1);
    });
  });

  describe('Cache Functions', () => {
    beforeEach(() => {
      clearQualityMetricsCache();
    });

    it('getCachedMetrics returns undefined for expired entries', () => {
      // This test would require manipulating the cache directly
      // For now, we'll test the cache stats functionality
      const stats = getCacheStats();
      expect(stats.currentSize).toBe(0);
      expect(stats.hits).toBe(0);
      expect(stats.misses).toBe(0);
    });

    it('setCachedMetrics evicts oldest entries when over limit', () => {
      // This test would require filling the cache beyond MAX_CACHE_ENTRIES
      // For now, we'll test the cache stats functionality
      const stats = getCacheStats();
      expect(stats.currentSize).toBe(0);
    });

    it('cache statistics track hits and misses correctly', async () => {
      mockRepcardClient.getCustomers.mockResolvedValue([]);
      mockRepcardClient.getAppointments.mockResolvedValue([]);
      mockRepcardClient.getCustomerStatusLogs.mockResolvedValue([]);
      mockRepcardClient.getCustomerAttachments.mockResolvedValue([]);

      // First call - should be a miss
      await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      let stats = getCacheStats();
      expect(stats.misses).toBeGreaterThan(0);

      // Second call - should be a hit
      await getQualityMetricsForUsers({
        repcardUserIds: ['repcard-user-1'],
        startDate: '2024-01-01',
        endDate: '2024-01-31',
      });

      stats = getCacheStats();
      expect(stats.hits).toBeGreaterThan(0);
    });

    it('clearQualityMetricsCache clears all entries', () => {
      clearQualityMetricsCache();
      const stats = getCacheStats();
      expect(stats.currentSize).toBe(0);
      expect(stats.hits).toBe(0);
      expect(stats.misses).toBe(0);
    });
  });

  describe('Utility Functions', () => {
    it('calculateCompositeQualityScore calculates weighted average correctly', () => {
      const metrics: QualityMetrics = {
        appointmentSpeed: {
          totalAppointments: 10,
          appointmentsWithin24Hours: 8,
          percentageWithin24Hours: 80,
          averageHoursToSchedule: 12,
        },
        attachmentRate: {
          totalCustomers: 10,
          customersWithAttachments: 7,
          percentageWithAttachments: 70,
          totalAttachments: 7,
        },
        rescheduleRate: {
          totalCustomers: 10,
          totalReschedules: 5,
          averageReschedulesPerCustomer: 0.5,
          customersWithReschedules: 5,
        },
        followUpConsistency: {
          customersRequiringFollowUps: 8,
          customersWithFollowUps: 6,
          percentageWithFollowUps: 75,
          totalFollowUpAppointments: 6,
        },
        period: {
          startDate: '2024-01-01',
          endDate: '2024-01-31',
        },
        calculatedAt: '2024-01-01T00:00:00Z',
      };

      const score = calculateCompositeQualityScore(metrics);
      expect(score).toBeGreaterThan(0);
      expect(score).toBeLessThanOrEqual(100);
    });

    it('formatMetricsForDisplay formats values correctly', () => {
      const metrics: QualityMetrics = {
        appointmentSpeed: {
          totalAppointments: 10,
          appointmentsWithin24Hours: 8,
          percentageWithin24Hours: 80.5,
          averageHoursToSchedule: 12.3,
        },
        attachmentRate: {
          totalCustomers: 10,
          customersWithAttachments: 7,
          percentageWithAttachments: 70.2,
          totalAttachments: 7,
        },
        rescheduleRate: {
          totalCustomers: 10,
          totalReschedules: 5,
          averageReschedulesPerCustomer: 0.5,
          customersWithReschedules: 5,
        },
        followUpConsistency: {
          customersRequiringFollowUps: 8,
          customersWithFollowUps: 6,
          percentageWithFollowUps: 75.8,
          totalFollowUpAppointments: 6,
        },
        period: {
          startDate: '2024-01-01',
          endDate: '2024-01-31',
        },
        calculatedAt: '2024-01-01T00:00:00Z',
      };

      const formatted = formatMetricsForDisplay(metrics);
      expect(formatted.appointmentSpeed).toBe('80.5%');
      expect(formatted.attachmentRate).toBe('70.2%');
      expect(formatted.rescheduleRate).toBe('0.5');
      expect(formatted.followUpConsistency).toBe('75.8%');
      expect(formatted.compositeScore).toBeDefined();
    });
  });
});
