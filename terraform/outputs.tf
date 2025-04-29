output "template_project_ecr_repository_url" {
  value = aws_ecr_repository.abatugo_autoscribe.repository_url
}

output "template_project_ecs_task_definition_arn" {
  value = aws_ecs_task_definition.abatugo_autoscribe_task.arn
}

output "template_project_ecs_task_definition_id" {
  value = aws_ecs_task_definition.abatugo_autoscribe_task.id
}