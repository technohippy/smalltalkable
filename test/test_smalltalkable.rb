require 'test/unit'
require 'smalltalkable'

class TestSmalltalkable < Test::Unit::TestCase
  def test_define_class
    assert_raise(NameError) do Counter end
    Object.subclass :Counter,
      instanceVariableNames: 'counterValue',
      classVariableNames: 'instanceCount',
      poolDictionaries: '',
      category: 'Category-Name'
    assert_nothing_raised do Counter end
    assert_equal Class, Counter.class
    assert_equal [:@counterValue, :@category], Counter.instance_variables
    assert_equal [:@@instanceCount], Counter.class_variables
    assert_equal 'Category-Name', Counter.category
  end

  def test_define_method
    Counter.compile '
      initialize value
        @counterValue = value'
    Counter.compile '
      counterValue
        @counterValue'
    Counter.compile '
      setCounterValue newValue
        @counterValue = newValue'
    Counter.compile '
      method a1, arg2:a2, arg3:a3
        [a1, a2, a3]'

    counter = Counter.new 0
    assert_equal 0, counter.counterValue
    counter.setCounterValue 1
    assert_equal 1, counter.counterValue
    assert_equal [1, 2, 3], counter.method(1, arg2:2, arg3:3)
    assert_equal [1, 2, 3], counter.method(1, arg3:3, arg2:2)
    assert_equal [1, 2, nil], counter.method(1, arg2:2)
    assert_equal [1, nil, 3], counter.method(1, arg3:3)
  end

  def test_define_class_method
    Counter.class.compile '
      classMethod
        "classMethod"'
    assert_equal 'classMethod', Counter.classMethod
  end

  def test_smalltalkize
    eval <<-EOS
      class ::Counter
        def method0
          []
        end
        smalltalkize :method0

        def method1(arg1)
          [arg1]
        end
        smalltalkize :method1

        def method2(arg1, arg2)
          [arg1, arg2]
        end
        smalltalkize :method2, :arg2

        def method3(arg1, arg2, arg3)
          [arg1, arg2, arg3]
        end
        smalltalkize :method3, :arg2, :arg3
      end
    EOS
    counter = Counter.new 0
    assert_equal [], counter.method0
    assert_equal [1], counter.method1(1)
    assert_equal [1, 2], counter.method2(1, arg2:2)
    assert_equal [1, 2, 3], counter.method3(1, arg2:2, arg3:3)

    String.smalltalkize :center, :padding
    assert_equal '***foo****', 'foo'.center(10, padding:'*')

    String.smalltalkize :rjust => [:rjustWidth, :padding]
    assert_equal '*******foo', 'foo'.rjustWidth(10, padding:'*')

    Time.singleton_class.smalltalkize :gm, :month, :day, :hour, :minute, :second
    time = Time.gm 2013, month:1, day:2, hour:3, minute:4, second:5
    assert_equal 2013, time.year
    assert_equal 1, time.month
    assert_equal 2, time.day
    assert_equal 3, time.hour
    assert_equal 4, time.min
    assert_equal 5, time.sec

    Time.singleton_class.smalltalkize :local => [:local_year, :month, :day, :hour, :minute, :second]
    local_time = Time.local_year 2013, month:1, day:2, hour:3, minute:4, second:5
    assert_equal 2013, local_time.year
    assert_equal 1, local_time.month
    assert_equal 2, local_time.day
    assert_equal 3, local_time.hour
    assert_equal 4, local_time.min
    assert_equal 5, local_time.sec
  end

  def test_if
    assert_equal :true, (1 == 1).if_true(->{:true}, if_false:->{:false})
    assert_equal :false, (0 == 1).if_true(->{:true}, if_false:->{:false})
    assert_equal :true, (1 == 1).if_true(->{:true})
    assert_nil (0 == 1).if_true(->{:true})

    assert_equal :true, (1 == 1).if_false(->{:false}, if_true:->{:true})
    assert_equal :false, (0 == 1).if_false(->{:false}, if_true:->{:true})
    assert_nil (1 == 1).if_false(->{:false})
    assert_equal :false, (0 == 1).if_false(->{:false})
  end

  def test_if_nil
    assert_equal :true, nil.if_nil(->{:true}, if_not_nil:->{:false})
    assert_equal :false, 1.if_nil(->{:true}, if_not_nil:->{:false})
    assert_equal :true, nil.if_nil(->{:true})
    assert_nil 1.if_nil(->{:true})
  end

  def test_while
    ret = ''
    c = 9
    ->{0 < c}.while_true ->{
      ret += c.to_s
      c -= 1
    }
    assert_equal '987654321', ret
  end
end
