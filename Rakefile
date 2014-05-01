task :default => :run_all

Dir["???"].each do |f|
  if File.directory? f then
    script = Dir["#{f}/main.*"].first

    if script then
      case script
      when /\.rb$/ then
        task f do
          out = "#{f}/output.txt"
          if not File.file?(out) or (File.mtime(out) < File.mtime(script)) then
            sh "(time ruby -Ilib #{script}) 2>&1 | tee #{out}"
          end
        end

        task :run_all => f
      else
        warn "Warning: unhandled script: #{script}"
      end
    end
  end
end
