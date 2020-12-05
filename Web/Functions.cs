using System.IO;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Azure.WebJobs;
using Microsoft.Azure.WebJobs.Extensions.Http;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;

namespace AdventOfCode2020.Web
{
    public static class Functions
    {
        [FunctionName("Day1Part1")]
        public static async Task<IActionResult> Day1(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.solve(1, 1, requestBody));
        }

        // TODO Refactor to use dynamic solver bindings

        [FunctionName("Day1Part2")]
        public static async Task<IActionResult> Day1Part2(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.solve(1, 2, requestBody));
        }

        [FunctionName("Day2Part1")]
        public static async Task<IActionResult> Day2Part1(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.solve(2, 1, requestBody));
        }

        [FunctionName("Day2Part2")]
        public static async Task<IActionResult> Day2Part2(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.solve(2, 2, requestBody));
        }

        [FunctionName("Day3Part1")]
        public static async Task<IActionResult> Day3Part1(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.solve(3, 1, requestBody));
        }

        [FunctionName("Day3Part2")]
        public static async Task<IActionResult> Day3Part2(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.solve(3, 2, requestBody));
        }
    }
}
