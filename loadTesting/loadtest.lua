local http = require("resty.http") -- Import the resty.http module

-- Configuration
local target_url = "http://localhost:8032/todo/create" -- Target URL
local total_requests = 10_000_000 -- Total number of requests
local num_threads = 1-- Number of concurrent threads (adjust based on system capacity)
local num_requests_per_thread = math.ceil(total_requests / num_threads) -- Requests per thread

-- Shared counters for metrics
local success_count = 0
local fail_count = 0

local function generate_request_body(counter)
    return string.format(
        [[
        {
            "task": "aditya-testing-%d",
            "description": "testing inMemoryQueue"
        }
        ]],
        counter
    )
end

-- Function to send a single HTTP request
local function send_request(counter)
    local httpc = http.new()
    -- local start_time = ngx.now()
    local res, err = httpc:request_uri(target_url, {
        method = "POST", -- HTTP method
        body = generate_request_body(counter) , -- Request body
        headers = {
            ["Content-Type"] = "application/json",
        },
        ssl_verify = false, -- Skip SSL verification (set to true for production)
    })
    local latency = ngx.now() - start_time
    if not res then
        ngx.log(ngx.ERR, "Request failed: ", err)
        return false
    end

    ngx.log(ngx.INFO, "Response Status: ", res.status)
    return true
end

-- Function to handle multiple requests in a thread
local function handle_requests_in_thread()
    for i = 1, num_requests_per_thread do
        local success = send_request(i)
        if success then
            success_count = success_count + 1
        else
            fail_count = fail_count + 1
        end
    end
end

-- Main: Spawn threads to handle requests
local threads = {}

for i = 1, num_threads do
    threads[i] = ngx.thread.spawn(handle_requests_in_thread)
end

-- Wait for all threads to complete
for _, thread in ipairs(threads) do
    local ok, err = ngx.thread.wait(thread)
    if not ok then
        ngx.log(ngx.ERR, "Thread error: ", err)
    end
end

-- Print results
ngx.say("All requests completed")
ngx.say("Total Requests: ", total_requests)
ngx.say("Success Count: ", success_count)
ngx.say("Fail Count: ", fail_count)
