# AI Hold Resolution Assistant - Test Examples

## Test Hold Reasons and Expected AI Outputs

### 1. Finance Hold Example

**Input Hold Reason:**
```
Customer needs to provide updated bank statements for loan approval. 
Lender requires additional documentation before proceeding with funding.
```

**Expected AI Output:**
```json
{
  "category": "Finance Hold",
  "confidence": 0.92,
  "rootCause": "Missing financial documentation required by lender for loan approval",
  "actions": [
    "Contact customer immediately to request updated bank statements",
    "Send lender's specific documentation requirements to customer",
    "Schedule follow-up call within 24 hours to confirm document submission",
    "Coordinate with lender to expedite review once documents are received",
    "Set calendar reminder to check status in 3 business days"
  ],
  "stakeholders": ["customer", "lender", "finance_team"],
  "estimatedResolution": "3-5 business days",
  "riskLevel": "medium",
  "urgency": "high",
  "nextMilestone": "Funding Approval",
  "preventionTips": [
    "Collect all financial documents during initial intake",
    "Verify document completeness before submitting to lender",
    "Set up automated reminders for document expiration dates"
  ]
}
```

### 2. HOA Hold Example

**Input Hold Reason:**
```
HOA board meeting scheduled for next month to review solar installation. 
Need approval before proceeding with design and permitting.
```

**Expected AI Output:**
```json
{
  "category": "HOA Hold",
  "confidence": 0.88,
  "rootCause": "HOA approval required before proceeding with solar installation",
  "actions": [
    "Contact HOA board to confirm meeting date and agenda",
    "Prepare comprehensive solar installation proposal for HOA review",
    "Offer to attend HOA meeting to answer questions directly",
    "Prepare alternative system designs if initial proposal is rejected",
    "Coordinate with customer to gather any required HOA application materials"
  ],
  "stakeholders": ["customer", "HOA_board", "design_team"],
  "estimatedResolution": "7-14 days",
  "riskLevel": "high",
  "urgency": "medium",
  "nextMilestone": "HOA Approval",
  "preventionTips": [
    "Check HOA requirements during initial site survey",
    "Submit HOA applications early in the process",
    "Build relationships with HOA board members"
  ]
}
```

### 3. Roof Hold Example

**Input Hold Reason:**
```
Roof inspection revealed structural issues that need to be addressed 
before solar installation can proceed. Customer needs to hire contractor.
```

**Expected AI Output:**
```json
{
  "category": "Roof Hold",
  "confidence": 0.95,
  "rootCause": "Structural roof issues must be resolved before solar installation",
  "actions": [
    "Provide customer with list of recommended roofing contractors",
    "Schedule follow-up inspection after roof repairs are completed",
    "Update system design if roof modifications affect panel placement",
    "Coordinate timeline with customer's roofing contractor",
    "Prepare revised installation schedule based on roof repair timeline"
  ],
  "stakeholders": ["customer", "roofing_contractor", "install_team"],
  "estimatedResolution": "2-4 weeks",
  "riskLevel": "high",
  "urgency": "medium",
  "nextMilestone": "Roof Repair Complete",
  "preventionTips": [
    "Conduct thorough roof inspection during initial site survey",
    "Identify potential roof issues early in the process",
    "Build relationships with local roofing contractors"
  ]
}
```

### 4. Customer Hold Example

**Input Hold Reason:**
```
Customer is reconsidering the system size and wants to discuss options 
before moving forward. They're concerned about the cost.
```

**Expected AI Output:**
```json
{
  "category": "Customer Hold",
  "confidence": 0.85,
  "rootCause": "Customer has concerns about system size and cost",
  "actions": [
    "Schedule in-person meeting to discuss system size options",
    "Prepare cost comparison for different system sizes",
    "Calculate ROI and payback period for each option",
    "Address customer's specific cost concerns with financing options",
    "Provide references from similar customers with different system sizes"
  ],
  "stakeholders": ["customer", "sales_rep", "finance_team"],
  "estimatedResolution": "1-2 weeks",
  "riskLevel": "medium",
  "urgency": "high",
  "nextMilestone": "Customer Decision",
  "preventionTips": [
    "Present multiple system size options during initial proposal",
    "Address cost concerns proactively with financing options",
    "Provide clear ROI calculations upfront"
  ]
}
```

### 5. Permitting Hold Example

**Input Hold Reason:**
```
Permit application submitted but AHJ requires additional engineering 
calculations for roof load analysis.
```

**Expected AI Output:**
```json
{
  "category": "Permitting Hold",
  "confidence": 0.90,
  "rootCause": "Additional engineering calculations required by Authority Having Jurisdiction",
  "actions": [
    "Contact engineering team to prepare roof load analysis",
    "Coordinate with AHJ to understand specific calculation requirements",
    "Submit additional engineering documentation within 48 hours",
    "Follow up with AHJ in 3 business days for status update",
    "Prepare backup plan if additional requirements are needed"
  ],
  "stakeholders": ["engineering_team", "AHJ", "permit_coordinator"],
  "estimatedResolution": "5-7 business days",
  "riskLevel": "medium",
  "urgency": "high",
  "nextMilestone": "Permit Approval",
  "preventionTips": [
    "Include comprehensive engineering calculations in initial permit application",
    "Build relationships with local AHJ officials",
    "Maintain library of approved engineering calculations"
  ]
}
```

## Edge Case Examples

### 1. Empty Hold Reason
**Input:** `""`
**Expected:** Fallback analysis with generic actions

### 2. Very Long Hold Reason
**Input:** `"Customer needs to provide updated bank statements for loan approval. Lender requires additional documentation before proceeding with funding. Also need to coordinate with HOA for approval. Roof inspection scheduled for next week. Customer is traveling and won't be available until next month."`
**Expected:** AI should identify multiple hold types and prioritize actions

### 3. Non-English Hold Reason
**Input:** `"Cliente necesita proporcionar estados bancarios actualizados"`
**Expected:** AI should still provide analysis, possibly with lower confidence

### 4. Technical Jargon
**Input:** `"NEM 2.0 application pending, need to submit revised single-line diagram with updated inverter specifications"`
**Expected:** AI should understand solar industry terminology

## Performance Test Scenarios

### 1. Concurrent Users
- Test with 10+ users analyzing holds simultaneously
- Verify rate limiting works correctly
- Check cache effectiveness

### 2. Large Hold Reasons
- Test with hold reasons >1000 characters
- Verify token limits are respected
- Check response time impact

### 3. Batch Analysis
- Test analyzing 5+ projects simultaneously
- Verify batch processing efficiency
- Check individual project analysis quality

## User Experience Test Cases

### 1. Loading States
- [ ] Skeleton loader appears immediately
- [ ] Loading state shows for 1-3 seconds
- [ ] Error states are user-friendly
- [ ] Retry functionality works

### 2. Interaction Testing
- [ ] Copy action button works
- [ ] Take action button triggers callback
- [ ] Feedback buttons submit successfully
- [ ] Refresh button updates analysis

### 3. Responsive Design
- [ ] Works on iPad (primary device)
- [ ] Works on desktop browsers
- [ ] Works on mobile phones
- [ ] Touch targets are 44px minimum

## Cost Analysis Examples

### Daily Usage Estimates
- **50 active users**
- **Average 2 hold analyses per user per day**
- **100 total analyses per day**
- **$0.01 per analysis**
- **$1.00 daily cost**
- **$30 monthly cost**

### Cost Optimization
- **Cache hit rate: 60%**
- **Effective cost per analysis: $0.004**
- **Daily cost with caching: $0.40**
- **Monthly cost with caching: $12**

## Success Metrics Examples

### Week 1 Targets
- **Response time: <3 seconds**
- **Success rate: >95%**
- **User adoption: >80%**
- **Helpfulness rating: >70%**

### Month 1 Targets
- **Hold resolution time: 50% reduction**
- **User satisfaction: >85%**
- **Cost efficiency: <$0.005 per analysis**
- **Cache hit rate: >70%**

## Troubleshooting Examples

### Common Issues

**Issue:** AI analysis not appearing
**Solution:** Check feature flags, verify user role permissions

**Issue:** Slow response times
**Solution:** Check OpenAI API status, verify caching is working

**Issue:** High costs
**Solution:** Review prompt efficiency, increase cache duration

**Issue:** Poor suggestion quality
**Solution:** Review user feedback, update prompts, add training data

### Error Messages

**"AI Analysis Unavailable"**
- Check internet connection
- Verify OpenAI API key
- Try refreshing the page

**"Feature not enabled for your role"**
- Contact your manager
- Check with IT support
- Verify user permissions

**"Analysis failed"**
- Try again in a few minutes
- Contact support if issue persists
- Check project data completeness

## Integration Testing

### 1. With Existing Hold Management
- [ ] AI card appears above hold management
- [ ] Toggle works correctly
- [ ] No conflicts with existing functionality
- [ ] Consistent styling and UX

### 2. With Offline Mode
- [ ] Cached analysis shows when offline
- [ ] New analysis queued when online
- [ ] Offline indicator works
- [ ] Sync works when reconnected

### 3. With User Permissions
- [ ] Only authorized users see AI
- [ ] Role-based access works
- [ ] Feature flags respected
- [ ] A/B testing functions correctly

---

## Quick Test Commands

```bash
# Test AI analysis endpoint
curl -X POST http://localhost:3000/api/ai/analyze-hold \
  -H "Content-Type: application/json" \
  -d '{"projectId": "123", "forceRefresh": true}'

# Test batch analysis
curl -X POST http://localhost:3000/api/ai/batch-analyze \
  -H "Content-Type: application/json" \
  -d '{"projectIds": ["123", "456", "789"]}'

# Test feedback submission
curl -X POST http://localhost:3000/api/ai/feedback \
  -H "Content-Type: application/json" \
  -d '{"projectId": "123", "analysisId": "456", "feedback": "helpful"}'
```

## Manual Testing Checklist

- [ ] Login as different user roles
- [ ] Navigate to project on hold
- [ ] Verify AI analysis appears
- [ ] Test all action buttons
- [ ] Submit feedback
- [ ] Test offline/online scenarios
- [ ] Verify caching behavior
- [ ] Test error handling
- [ ] Check responsive design
- [ ] Verify accessibility
