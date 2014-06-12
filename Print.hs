module Print where

import UU.PPrint
import Lambda

showId :: Term -> Doc
showId (Id v) = text v

showAbs :: Term -> Doc
showAbs (Abs v t0) = text "λ " <> text v <> text " →" 
                   <> group (nest 3 $ line <> showTerm t0)

showApp :: Term -> Doc
showApp (App t1 t2) = showTerm t1 <> text "@" <> showTerm' t2

showTerm' :: Term -> Doc
showTerm' t@(Id _)    = showId t
showTerm' t@(Abs _ _) = text "(" <> showAbs t <> text ")"
showTerm' t@(App _ _) = text "(" <> showApp t <> text ")"

showTerm :: Term -> Doc
showTerm t@(Id _)    = showId t
showTerm t@(Abs _ _) = showAbs t
showTerm t@(App _ _) = showApp t

pprint :: Term -> String
pprint t = displayS (renderPretty 1 20 $ showTerm t) $ ""
