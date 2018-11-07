module Luna.Manager.Legal where

import Prologue

companyName, copyright, productName, productDescription
	:: (IsString a, Semigroup a) => a
companyName        = "New Byte Order Sp. z o. o."
copyright          = "© 2018 " <> companyName
productName        = "Luna Studio"
productDescription = "Luna Studio"
