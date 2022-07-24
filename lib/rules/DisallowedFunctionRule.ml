module Implementation : HasRule.HasRule = struct
  let meta =
    { HasRule.ruleName = "DisallowedFunction"
    ; HasRule.ruleDescription = "Disallow certain functions from running"
    }
  let lint _ast = ()
end

module Rule = HasRule.MakeRule (Implementation)
