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

math = "/MyApplications/Mathematica.app/Contents/MacOS/MathKernel"

task :export do
  system %(echo 'With[{dir = "/Users/ryan/Work/git/euler/"}, Export[dir <> "Euler.m", Import[dir <> "Euler.nb", {"NB", "Plaintext"}], "Text"]]' | '#{math}' -noprompt)
end
