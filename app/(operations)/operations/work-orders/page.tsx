export default function WorkOrdersPage() {
  return (
    <div className="space-y-4 mobile:space-y-6">
      <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
        <div>
          <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
            Work Orders
          </h1>
          <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
            Manage and track work orders across all projects
          </p>
        </div>
      </div>

      <div className="p-6 border rounded-lg bg-white">
        <div className="text-center py-12">
          <h3 className="text-lg font-semibold text-gray-900 mb-2">Coming Soon</h3>
          <p className="text-gray-600">
            Work order management features are currently under development.
          </p>
        </div>
      </div>
    </div>
  );
}
