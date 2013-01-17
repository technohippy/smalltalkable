require "smalltalkable/version"

module Smalltalkable
end

class Class
  attr_accessor :category

  def subclass(name, opt={})
    klass = Class.new(self)
    if opt[:instanceVariableNames]
      opt[:instanceVariableNames].split(' ').each do |ivar|
        klass.instance_variable_set "@#{ivar}", nil
      end
    end
    if opt[:classVariableNames]
      opt[:classVariableNames].split(' ').each do |cvar|
        klass.class_variable_set "@@#{cvar}", nil
      end
    end
    if opt[:poolDictionaries] && !opt[:poolDictionaries].empty?
      raise NotImplementedError.new('pool dictionary is not available') 
    end
    if opt[:category]
      klass.category = opt[:category] unless opt[:category].empty?
    end
    Object.const_set name, klass
  end

  def compile(code)
    sig, method = code.strip.split "\n", 2
    name, str_arg = sig.split /\s+/, 2
    args = []
    pairs = {}
    if str_arg
      first_arg, other_args = str_arg.split /\s*,\s*/, 2
      args.push first_arg
      if other_args
        args.push 'opts={}'
        other_args.split(/\s+/).each do |pair|
          key, value = pair.split /\s*:\s*/
          pairs[key] = value
        end
      end
    end

    self.class_eval <<-EOS
      def #{name.strip}(#{args.join ', '})
        #{pairs.map do |k, v| "#{v} = opts[:#{k}]" end.join "\n"}
        #{method}
      end
    EOS
  end
end

class TrueClass
  def if_true(true_proc, opts={})
    true_proc.call
  end
  alias ifTrue if_true

  def if_false(false_proc, opts={})
    (opts[:if_true] || opts[:ifTrue] || ->{nil}).call
  end
  alias ifFalse if_false
end

class FalseClass
  def if_true(true_proc, opts={})
    (opts[:if_false] || opts[:ifFalse] || ->{nil}).call
  end
  alias ifTrue if_true

  def if_false(false_proc, opts={})
    false_proc.call
  end
  alias ifFalse if_false
end

class NilClass
  def if_nil(true_proc, opts={})
    true_proc.call
  end
  alias ifNil if_nil

  def if_not_nil(true_proc, opts={})
    (opts[:if_nil] || opts[:ifNil] || ->{nil}).call
  end
  alias ifNotNil if_not_nil
end

class Object
  def if_nil(true_proc, opts={})
    (opts[:if_not_nil] || opts[:ifNotNil] || ->{nil}).call
  end
  alias ifNil if_nil

  def if_not_nil(true_proc, opts={})
    true_proc.call
  end
  alias ifNotNil if_not_nil
end

class Proc
  def while_true(block)
    if self.call
      block.call
      while_true block
    end
  end
  alias whileTrue while_true

  alias value call
end
