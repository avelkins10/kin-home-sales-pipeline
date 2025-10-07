interface MilestoneConnectorProps {
  status: 'complete' | 'pending'
}

export function MilestoneConnector({ status }: MilestoneConnectorProps) {
  return (
    <div
      className={`w-12 h-1 mt-8 transition-colors duration-300 ${
        status === 'complete' ? 'bg-green-500' : 'bg-gray-300'
      }`}
    />
  )
}
