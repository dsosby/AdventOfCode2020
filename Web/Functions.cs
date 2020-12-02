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
        [FunctionName("Day1")]
        public static async Task<IActionResult> Day1(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.Day1(requestBody));
        }

        [FunctionName("Day1Part2")]
        public static async Task<IActionResult> Day1Part2(
            [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
            return new OkObjectResult(Solutions.Day1Part2(requestBody));
        }
    }
}
