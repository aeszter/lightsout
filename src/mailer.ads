with CM.Taint;

package Mailer is
   procedure Send (Node_Name : CM.Taint.Trusted_String; Message : String);
end Mailer;
