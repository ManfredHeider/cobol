      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.07.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0026.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DBVB0026.CPv  $
      *
      *   Rev 2.3   Feb 18 2021 09:29:52   t13ae56
      *AVB01620: PGM - Rückdatierung für KTY 289 zulassen (VB37)
      *
      *   Rev 2.2   Jun 30 2020 08:01:54   t13ex57
      *AVB01620: Anpassung VB37 -PFLEGE MITARBEITER-STATUS / Plausi
      *s und Rückdatierung
      *
      *   Rev 2.1   Jul 25 2003 09:35:48   t13ex22
      *AVB00693: Prüfziffer ergänzt
      *
      *   Rev 2.0   Jul 23 2003 16:33:32   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus Enterprise Developer for Eclipse  5.0.00256
      *   Micro Focus BMS Screen Painter (ver BMSIDE_5-0-01)
      *   MapSet Name   BVB0026
      *   Date Created  05/20/2020
      *   Time Created  14:32:55

      *  Input Data For Map BVB0026
         01 BVB0026I.
            03 FILLER                         PIC X(12).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER REDEFINES ESTPRZF.
               05 ESTPRZA                        PIC X.
            03 ESTPRZI                        PIC X(1).
            03 E1FCL                          PIC S9(4) COMP.
            03 E1FCF                          PIC X.
            03 FILLER REDEFINES E1FCF.
               05 E1FCA                          PIC X.
            03 E1FCI                          PIC X(1).
            03 E1STNRL                        PIC S9(4) COMP.
            03 E1STNRF                        PIC X.
            03 FILLER REDEFINES E1STNRF.
               05 E1STNRA                        PIC X.
            03 E1STNRI                        PIC X(7).
            03 E1PRZL                         PIC S9(4) COMP.
            03 E1PRZF                         PIC X.
            03 FILLER REDEFINES E1PRZF.
               05 E1PRZA                         PIC X.
            03 E1PRZI                         PIC X(1).
            03 A1NAMEL                        PIC S9(4) COMP.
            03 A1NAMEF                        PIC X.
            03 FILLER REDEFINES A1NAMEF.
               05 A1NAMEA                        PIC X.
            03 A1NAMEI                        PIC X(24).
            03 A1STFL                         PIC S9(4) COMP.
            03 A1STFF                         PIC X.
            03 FILLER REDEFINES A1STFF.
               05 A1STFA                         PIC X.
            03 A1STFI                         PIC X(4).
            03 E1KNRL                         PIC S9(4) COMP.
            03 E1KNRF                         PIC X.
            03 FILLER REDEFINES E1KNRF.
               05 E1KNRA                         PIC X.
            03 E1KNRI                         PIC X(3).
            03 A1BEZL                         PIC S9(4) COMP.
            03 A1BEZF                         PIC X.
            03 FILLER REDEFINES A1BEZF.
               05 A1BEZA                         PIC X.
            03 A1BEZI                         PIC X(10).
            03 E1DATVL                        PIC S9(4) COMP.
            03 E1DATVF                        PIC X.
            03 FILLER REDEFINES E1DATVF.
               05 E1DATVA                        PIC X.
            03 E1DATVI                        PIC X(10).
            03 A1DATBL                        PIC S9(4) COMP.
            03 A1DATBF                        PIC X.
            03 FILLER REDEFINES A1DATBF.
               05 A1DATBA                        PIC X.
            03 A1DATBI                        PIC X(10).
            03 E2FCL                          PIC S9(4) COMP.
            03 E2FCF                          PIC X.
            03 FILLER REDEFINES E2FCF.
               05 E2FCA                          PIC X.
            03 E2FCI                          PIC X(1).
            03 E2STNRL                        PIC S9(4) COMP.
            03 E2STNRF                        PIC X.
            03 FILLER REDEFINES E2STNRF.
               05 E2STNRA                        PIC X.
            03 E2STNRI                        PIC X(7).
            03 E2PRZL                         PIC S9(4) COMP.
            03 E2PRZF                         PIC X.
            03 FILLER REDEFINES E2PRZF.
               05 E2PRZA                         PIC X.
            03 E2PRZI                         PIC X(1).
            03 A2NAMEL                        PIC S9(4) COMP.
            03 A2NAMEF                        PIC X.
            03 FILLER REDEFINES A2NAMEF.
               05 A2NAMEA                        PIC X.
            03 A2NAMEI                        PIC X(24).
            03 A2STFL                         PIC S9(4) COMP.
            03 A2STFF                         PIC X.
            03 FILLER REDEFINES A2STFF.
               05 A2STFA                         PIC X.
            03 A2STFI                         PIC X(4).
            03 E2KNRL                         PIC S9(4) COMP.
            03 E2KNRF                         PIC X.
            03 FILLER REDEFINES E2KNRF.
               05 E2KNRA                         PIC X.
            03 E2KNRI                         PIC X(3).
            03 A2BEZL                         PIC S9(4) COMP.
            03 A2BEZF                         PIC X.
            03 FILLER REDEFINES A2BEZF.
               05 A2BEZA                         PIC X.
            03 A2BEZI                         PIC X(10).
            03 E2DATVL                        PIC S9(4) COMP.
            03 E2DATVF                        PIC X.
            03 FILLER REDEFINES E2DATVF.
               05 E2DATVA                        PIC X.
            03 E2DATVI                        PIC X(10).
            03 A2DATBL                        PIC S9(4) COMP.
            03 A2DATBF                        PIC X.
            03 FILLER REDEFINES A2DATBF.
               05 A2DATBA                        PIC X.
            03 A2DATBI                        PIC X(10).
            03 E3FCL                          PIC S9(4) COMP.
            03 E3FCF                          PIC X.
            03 FILLER REDEFINES E3FCF.
               05 E3FCA                          PIC X.
            03 E3FCI                          PIC X(1).
            03 E3STNRL                        PIC S9(4) COMP.
            03 E3STNRF                        PIC X.
            03 FILLER REDEFINES E3STNRF.
               05 E3STNRA                        PIC X.
            03 E3STNRI                        PIC X(7).
            03 E3PRZL                         PIC S9(4) COMP.
            03 E3PRZF                         PIC X.
            03 FILLER REDEFINES E3PRZF.
               05 E3PRZA                         PIC X.
            03 E3PRZI                         PIC X(1).
            03 A3NAMEL                        PIC S9(4) COMP.
            03 A3NAMEF                        PIC X.
            03 FILLER REDEFINES A3NAMEF.
               05 A3NAMEA                        PIC X.
            03 A3NAMEI                        PIC X(24).
            03 A3STFL                         PIC S9(4) COMP.
            03 A3STFF                         PIC X.
            03 FILLER REDEFINES A3STFF.
               05 A3STFA                         PIC X.
            03 A3STFI                         PIC X(4).
            03 E3KNRL                         PIC S9(4) COMP.
            03 E3KNRF                         PIC X.
            03 FILLER REDEFINES E3KNRF.
               05 E3KNRA                         PIC X.
            03 E3KNRI                         PIC X(3).
            03 A3BEZL                         PIC S9(4) COMP.
            03 A3BEZF                         PIC X.
            03 FILLER REDEFINES A3BEZF.
               05 A3BEZA                         PIC X.
            03 A3BEZI                         PIC X(10).
            03 E3DATVL                        PIC S9(4) COMP.
            03 E3DATVF                        PIC X.
            03 FILLER REDEFINES E3DATVF.
               05 E3DATVA                        PIC X.
            03 E3DATVI                        PIC X(10).
            03 A3DATBL                        PIC S9(4) COMP.
            03 A3DATBF                        PIC X.
            03 FILLER REDEFINES A3DATBF.
               05 A3DATBA                        PIC X.
            03 A3DATBI                        PIC X(10).
            03 E4FCL                          PIC S9(4) COMP.
            03 E4FCF                          PIC X.
            03 FILLER REDEFINES E4FCF.
               05 E4FCA                          PIC X.
            03 E4FCI                          PIC X(1).
            03 E4STNRL                        PIC S9(4) COMP.
            03 E4STNRF                        PIC X.
            03 FILLER REDEFINES E4STNRF.
               05 E4STNRA                        PIC X.
            03 E4STNRI                        PIC X(7).
            03 E4PRZL                         PIC S9(4) COMP.
            03 E4PRZF                         PIC X.
            03 FILLER REDEFINES E4PRZF.
               05 E4PRZA                         PIC X.
            03 E4PRZI                         PIC X(1).
            03 A4NAMEL                        PIC S9(4) COMP.
            03 A4NAMEF                        PIC X.
            03 FILLER REDEFINES A4NAMEF.
               05 A4NAMEA                        PIC X.
            03 A4NAMEI                        PIC X(24).
            03 A4STFL                         PIC S9(4) COMP.
            03 A4STFF                         PIC X.
            03 FILLER REDEFINES A4STFF.
               05 A4STFA                         PIC X.
            03 A4STFI                         PIC X(4).
            03 E4KNRL                         PIC S9(4) COMP.
            03 E4KNRF                         PIC X.
            03 FILLER REDEFINES E4KNRF.
               05 E4KNRA                         PIC X.
            03 E4KNRI                         PIC X(3).
            03 A4BEZL                         PIC S9(4) COMP.
            03 A4BEZF                         PIC X.
            03 FILLER REDEFINES A4BEZF.
               05 A4BEZA                         PIC X.
            03 A4BEZI                         PIC X(10).
            03 E4DATVL                        PIC S9(4) COMP.
            03 E4DATVF                        PIC X.
            03 FILLER REDEFINES E4DATVF.
               05 E4DATVA                        PIC X.
            03 E4DATVI                        PIC X(10).
            03 A4DATBL                        PIC S9(4) COMP.
            03 A4DATBF                        PIC X.
            03 FILLER REDEFINES A4DATBF.
               05 A4DATBA                        PIC X.
            03 A4DATBI                        PIC X(10).
            03 E5FCL                          PIC S9(4) COMP.
            03 E5FCF                          PIC X.
            03 FILLER REDEFINES E5FCF.
               05 E5FCA                          PIC X.
            03 E5FCI                          PIC X(1).
            03 E5STNRL                        PIC S9(4) COMP.
            03 E5STNRF                        PIC X.
            03 FILLER REDEFINES E5STNRF.
               05 E5STNRA                        PIC X.
            03 E5STNRI                        PIC X(7).
            03 E5PRZL                         PIC S9(4) COMP.
            03 E5PRZF                         PIC X.
            03 FILLER REDEFINES E5PRZF.
               05 E5PRZA                         PIC X.
            03 E5PRZI                         PIC X(1).
            03 A5NAMEL                        PIC S9(4) COMP.
            03 A5NAMEF                        PIC X.
            03 FILLER REDEFINES A5NAMEF.
               05 A5NAMEA                        PIC X.
            03 A5NAMEI                        PIC X(24).
            03 A5STFL                         PIC S9(4) COMP.
            03 A5STFF                         PIC X.
            03 FILLER REDEFINES A5STFF.
               05 A5STFA                         PIC X.
            03 A5STFI                         PIC X(4).
            03 E5KNRL                         PIC S9(4) COMP.
            03 E5KNRF                         PIC X.
            03 FILLER REDEFINES E5KNRF.
               05 E5KNRA                         PIC X.
            03 E5KNRI                         PIC X(3).
            03 A5BEZL                         PIC S9(4) COMP.
            03 A5BEZF                         PIC X.
            03 FILLER REDEFINES A5BEZF.
               05 A5BEZA                         PIC X.
            03 A5BEZI                         PIC X(10).
            03 E5DATVL                        PIC S9(4) COMP.
            03 E5DATVF                        PIC X.
            03 FILLER REDEFINES E5DATVF.
               05 E5DATVA                        PIC X.
            03 E5DATVI                        PIC X(10).
            03 A5DATBL                        PIC S9(4) COMP.
            03 A5DATBF                        PIC X.
            03 FILLER REDEFINES A5DATBF.
               05 A5DATBA                        PIC X.
            03 A5DATBI                        PIC X(10).
            03 E6FCL                          PIC S9(4) COMP.
            03 E6FCF                          PIC X.
            03 FILLER REDEFINES E6FCF.
               05 E6FCA                          PIC X.
            03 E6FCI                          PIC X(1).
            03 E6STNRL                        PIC S9(4) COMP.
            03 E6STNRF                        PIC X.
            03 FILLER REDEFINES E6STNRF.
               05 E6STNRA                        PIC X.
            03 E6STNRI                        PIC X(7).
            03 E6PRZL                         PIC S9(4) COMP.
            03 E6PRZF                         PIC X.
            03 FILLER REDEFINES E6PRZF.
               05 E6PRZA                         PIC X.
            03 E6PRZI                         PIC X(1).
            03 A6NAMEL                        PIC S9(4) COMP.
            03 A6NAMEF                        PIC X.
            03 FILLER REDEFINES A6NAMEF.
               05 A6NAMEA                        PIC X.
            03 A6NAMEI                        PIC X(24).
            03 A6STFL                         PIC S9(4) COMP.
            03 A6STFF                         PIC X.
            03 FILLER REDEFINES A6STFF.
               05 A6STFA                         PIC X.
            03 A6STFI                         PIC X(4).
            03 E6KNRL                         PIC S9(4) COMP.
            03 E6KNRF                         PIC X.
            03 FILLER REDEFINES E6KNRF.
               05 E6KNRA                         PIC X.
            03 E6KNRI                         PIC X(3).
            03 A6BEZL                         PIC S9(4) COMP.
            03 A6BEZF                         PIC X.
            03 FILLER REDEFINES A6BEZF.
               05 A6BEZA                         PIC X.
            03 A6BEZI                         PIC X(10).
            03 E6DATVL                        PIC S9(4) COMP.
            03 E6DATVF                        PIC X.
            03 FILLER REDEFINES E6DATVF.
               05 E6DATVA                        PIC X.
            03 E6DATVI                        PIC X(10).
            03 A6DATBL                        PIC S9(4) COMP.
            03 A6DATBF                        PIC X.
            03 FILLER REDEFINES A6DATBF.
               05 A6DATBA                        PIC X.
            03 A6DATBI                        PIC X(10).
            03 E7FCL                          PIC S9(4) COMP.
            03 E7FCF                          PIC X.
            03 FILLER REDEFINES E7FCF.
               05 E7FCA                          PIC X.
            03 E7FCI                          PIC X(1).
            03 E7STNRL                        PIC S9(4) COMP.
            03 E7STNRF                        PIC X.
            03 FILLER REDEFINES E7STNRF.
               05 E7STNRA                        PIC X.
            03 E7STNRI                        PIC X(7).
            03 E7PRZL                         PIC S9(4) COMP.
            03 E7PRZF                         PIC X.
            03 FILLER REDEFINES E7PRZF.
               05 E7PRZA                         PIC X.
            03 E7PRZI                         PIC X(1).
            03 A7NAMEL                        PIC S9(4) COMP.
            03 A7NAMEF                        PIC X.
            03 FILLER REDEFINES A7NAMEF.
               05 A7NAMEA                        PIC X.
            03 A7NAMEI                        PIC X(24).
            03 A7STFL                         PIC S9(4) COMP.
            03 A7STFF                         PIC X.
            03 FILLER REDEFINES A7STFF.
               05 A7STFA                         PIC X.
            03 A7STFI                         PIC X(4).
            03 E7KNRL                         PIC S9(4) COMP.
            03 E7KNRF                         PIC X.
            03 FILLER REDEFINES E7KNRF.
               05 E7KNRA                         PIC X.
            03 E7KNRI                         PIC X(3).
            03 A7BEZL                         PIC S9(4) COMP.
            03 A7BEZF                         PIC X.
            03 FILLER REDEFINES A7BEZF.
               05 A7BEZA                         PIC X.
            03 A7BEZI                         PIC X(10).
            03 E7DATVL                        PIC S9(4) COMP.
            03 E7DATVF                        PIC X.
            03 FILLER REDEFINES E7DATVF.
               05 E7DATVA                        PIC X.
            03 E7DATVI                        PIC X(10).
            03 A7DATBL                        PIC S9(4) COMP.
            03 A7DATBF                        PIC X.
            03 FILLER REDEFINES A7DATBF.
               05 A7DATBA                        PIC X.
            03 A7DATBI                        PIC X(10).
            03 E8FCL                          PIC S9(4) COMP.
            03 E8FCF                          PIC X.
            03 FILLER REDEFINES E8FCF.
               05 E8FCA                          PIC X.
            03 E8FCI                          PIC X(1).
            03 E8STNRL                        PIC S9(4) COMP.
            03 E8STNRF                        PIC X.
            03 FILLER REDEFINES E8STNRF.
               05 E8STNRA                        PIC X.
            03 E8STNRI                        PIC X(7).
            03 E8PRZL                         PIC S9(4) COMP.
            03 E8PRZF                         PIC X.
            03 FILLER REDEFINES E8PRZF.
               05 E8PRZA                         PIC X.
            03 E8PRZI                         PIC X(1).
            03 A8NAMEL                        PIC S9(4) COMP.
            03 A8NAMEF                        PIC X.
            03 FILLER REDEFINES A8NAMEF.
               05 A8NAMEA                        PIC X.
            03 A8NAMEI                        PIC X(24).
            03 A8STFL                         PIC S9(4) COMP.
            03 A8STFF                         PIC X.
            03 FILLER REDEFINES A8STFF.
               05 A8STFA                         PIC X.
            03 A8STFI                         PIC X(4).
            03 E8KNRL                         PIC S9(4) COMP.
            03 E8KNRF                         PIC X.
            03 FILLER REDEFINES E8KNRF.
               05 E8KNRA                         PIC X.
            03 E8KNRI                         PIC X(3).
            03 A8BEZL                         PIC S9(4) COMP.
            03 A8BEZF                         PIC X.
            03 FILLER REDEFINES A8BEZF.
               05 A8BEZA                         PIC X.
            03 A8BEZI                         PIC X(10).
            03 E8DATVL                        PIC S9(4) COMP.
            03 E8DATVF                        PIC X.
            03 FILLER REDEFINES E8DATVF.
               05 E8DATVA                        PIC X.
            03 E8DATVI                        PIC X(10).
            03 A8DATBL                        PIC S9(4) COMP.
            03 A8DATBF                        PIC X.
            03 FILLER REDEFINES A8DATBF.
               05 A8DATBA                        PIC X.
            03 A8DATBI                        PIC X(10).
            03 E9FCL                          PIC S9(4) COMP.
            03 E9FCF                          PIC X.
            03 FILLER REDEFINES E9FCF.
               05 E9FCA                          PIC X.
            03 E9FCI                          PIC X(1).
            03 E9STNRL                        PIC S9(4) COMP.
            03 E9STNRF                        PIC X.
            03 FILLER REDEFINES E9STNRF.
               05 E9STNRA                        PIC X.
            03 E9STNRI                        PIC X(7).
            03 E9PRZL                         PIC S9(4) COMP.
            03 E9PRZF                         PIC X.
            03 FILLER REDEFINES E9PRZF.
               05 E9PRZA                         PIC X.
            03 E9PRZI                         PIC X(1).
            03 A9NAMEL                        PIC S9(4) COMP.
            03 A9NAMEF                        PIC X.
            03 FILLER REDEFINES A9NAMEF.
               05 A9NAMEA                        PIC X.
            03 A9NAMEI                        PIC X(24).
            03 A9STFL                         PIC S9(4) COMP.
            03 A9STFF                         PIC X.
            03 FILLER REDEFINES A9STFF.
               05 A9STFA                         PIC X.
            03 A9STFI                         PIC X(4).
            03 E9KNRL                         PIC S9(4) COMP.
            03 E9KNRF                         PIC X.
            03 FILLER REDEFINES E9KNRF.
               05 E9KNRA                         PIC X.
            03 E9KNRI                         PIC X(3).
            03 A9BEZL                         PIC S9(4) COMP.
            03 A9BEZF                         PIC X.
            03 FILLER REDEFINES A9BEZF.
               05 A9BEZA                         PIC X.
            03 A9BEZI                         PIC X(10).
            03 E9DATVL                        PIC S9(4) COMP.
            03 E9DATVF                        PIC X.
            03 FILLER REDEFINES E9DATVF.
               05 E9DATVA                        PIC X.
            03 E9DATVI                        PIC X(10).
            03 A9DATBL                        PIC S9(4) COMP.
            03 A9DATBF                        PIC X.
            03 FILLER REDEFINES A9DATBF.
               05 A9DATBA                        PIC X.
            03 A9DATBI                        PIC X(10).
            03 EAFCL                          PIC S9(4) COMP.
            03 EAFCF                          PIC X.
            03 FILLER REDEFINES EAFCF.
               05 EAFCA                          PIC X.
            03 EAFCI                          PIC X(1).
            03 EASTNRL                        PIC S9(4) COMP.
            03 EASTNRF                        PIC X.
            03 FILLER REDEFINES EASTNRF.
               05 EASTNRA                        PIC X.
            03 EASTNRI                        PIC X(7).
            03 EAPRZL                         PIC S9(4) COMP.
            03 EAPRZF                         PIC X.
            03 FILLER REDEFINES EAPRZF.
               05 EAPRZA                         PIC X.
            03 EAPRZI                         PIC X(1).
            03 AANAMEL                        PIC S9(4) COMP.
            03 AANAMEF                        PIC X.
            03 FILLER REDEFINES AANAMEF.
               05 AANAMEA                        PIC X.
            03 AANAMEI                        PIC X(24).
            03 AASTFL                         PIC S9(4) COMP.
            03 AASTFF                         PIC X.
            03 FILLER REDEFINES AASTFF.
               05 AASTFA                         PIC X.
            03 AASTFI                         PIC X(4).
            03 EAKNRL                         PIC S9(4) COMP.
            03 EAKNRF                         PIC X.
            03 FILLER REDEFINES EAKNRF.
               05 EAKNRA                         PIC X.
            03 EAKNRI                         PIC X(3).
            03 AABEZL                         PIC S9(4) COMP.
            03 AABEZF                         PIC X.
            03 FILLER REDEFINES AABEZF.
               05 AABEZA                         PIC X.
            03 AABEZI                         PIC X(10).
            03 EADATVL                        PIC S9(4) COMP.
            03 EADATVF                        PIC X.
            03 FILLER REDEFINES EADATVF.
               05 EADATVA                        PIC X.
            03 EADATVI                        PIC X(10).
            03 AADATBL                        PIC S9(4) COMP.
            03 AADATBF                        PIC X.
            03 FILLER REDEFINES AADATBF.
               05 AADATBA                        PIC X.
            03 AADATBI                        PIC X(10).
            03 EBFCL                          PIC S9(4) COMP.
            03 EBFCF                          PIC X.
            03 FILLER REDEFINES EBFCF.
               05 EBFCA                          PIC X.
            03 EBFCI                          PIC X(1).
            03 EBSTNRL                        PIC S9(4) COMP.
            03 EBSTNRF                        PIC X.
            03 FILLER REDEFINES EBSTNRF.
               05 EBSTNRA                        PIC X.
            03 EBSTNRI                        PIC X(7).
            03 EBPRZL                         PIC S9(4) COMP.
            03 EBPRZF                         PIC X.
            03 FILLER REDEFINES EBPRZF.
               05 EBPRZA                         PIC X.
            03 EBPRZI                         PIC X(1).
            03 ABNAMEL                        PIC S9(4) COMP.
            03 ABNAMEF                        PIC X.
            03 FILLER REDEFINES ABNAMEF.
               05 ABNAMEA                        PIC X.
            03 ABNAMEI                        PIC X(24).
            03 ABSTFL                         PIC S9(4) COMP.
            03 ABSTFF                         PIC X.
            03 FILLER REDEFINES ABSTFF.
               05 ABSTFA                         PIC X.
            03 ABSTFI                         PIC X(4).
            03 EBKNRL                         PIC S9(4) COMP.
            03 EBKNRF                         PIC X.
            03 FILLER REDEFINES EBKNRF.
               05 EBKNRA                         PIC X.
            03 EBKNRI                         PIC X(3).
            03 ABBEZL                         PIC S9(4) COMP.
            03 ABBEZF                         PIC X.
            03 FILLER REDEFINES ABBEZF.
               05 ABBEZA                         PIC X.
            03 ABBEZI                         PIC X(10).
            03 EBDATVL                        PIC S9(4) COMP.
            03 EBDATVF                        PIC X.
            03 FILLER REDEFINES EBDATVF.
               05 EBDATVA                        PIC X.
            03 EBDATVI                        PIC X(10).
            03 ABDATBL                        PIC S9(4) COMP.
            03 ABDATBF                        PIC X.
            03 FILLER REDEFINES ABDATBF.
               05 ABDATBA                        PIC X.
            03 ABDATBI                        PIC X(10).
            03 ECFCL                          PIC S9(4) COMP.
            03 ECFCF                          PIC X.
            03 FILLER REDEFINES ECFCF.
               05 ECFCA                          PIC X.
            03 ECFCI                          PIC X(1).
            03 ECSTNRL                        PIC S9(4) COMP.
            03 ECSTNRF                        PIC X.
            03 FILLER REDEFINES ECSTNRF.
               05 ECSTNRA                        PIC X.
            03 ECSTNRI                        PIC X(7).
            03 ECPRZL                         PIC S9(4) COMP.
            03 ECPRZF                         PIC X.
            03 FILLER REDEFINES ECPRZF.
               05 ECPRZA                         PIC X.
            03 ECPRZI                         PIC X(1).
            03 ACNAMEL                        PIC S9(4) COMP.
            03 ACNAMEF                        PIC X.
            03 FILLER REDEFINES ACNAMEF.
               05 ACNAMEA                        PIC X.
            03 ACNAMEI                        PIC X(24).
            03 ACSTFL                         PIC S9(4) COMP.
            03 ACSTFF                         PIC X.
            03 FILLER REDEFINES ACSTFF.
               05 ACSTFA                         PIC X.
            03 ACSTFI                         PIC X(4).
            03 ECKNRL                         PIC S9(4) COMP.
            03 ECKNRF                         PIC X.
            03 FILLER REDEFINES ECKNRF.
               05 ECKNRA                         PIC X.
            03 ECKNRI                         PIC X(3).
            03 ACBEZL                         PIC S9(4) COMP.
            03 ACBEZF                         PIC X.
            03 FILLER REDEFINES ACBEZF.
               05 ACBEZA                         PIC X.
            03 ACBEZI                         PIC X(10).
            03 ECDATVL                        PIC S9(4) COMP.
            03 ECDATVF                        PIC X.
            03 FILLER REDEFINES ECDATVF.
               05 ECDATVA                        PIC X.
            03 ECDATVI                        PIC X(10).
            03 ACDATBL                        PIC S9(4) COMP.
            03 ACDATBF                        PIC X.
            03 FILLER REDEFINES ACDATBF.
               05 ACDATBA                        PIC X.
            03 ACDATBI                        PIC X(10).
            03 AINFOL                         PIC S9(4) COMP.
            03 AINFOF                         PIC X.
            03 FILLER REDEFINES AINFOF.
               05 AINFOA                         PIC X.
            03 AINFOI                         PIC X(40).

      *  Output Data For Map BVB0026
         01 BVB0026O REDEFINES BVB0026I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E1PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A1NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A1STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E1KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A1BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E1DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A1DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E2FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E2STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E2PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A2NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A2STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E2KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A2BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E2DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A2DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E3FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E3STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E3PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A3NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A3STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E3KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A3BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E3DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A3DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E4FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E4STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E4PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A4NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A4STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E4KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A4BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E4DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A4DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E5FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E5STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E5PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A5NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A5STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E5KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A5BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E5DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A5DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E6FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E6STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E6PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A6NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A6STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E6KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A6BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E6DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A6DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E7FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E7STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E7PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A7NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A7STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E7KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A7BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E7DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A7DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E8FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E8STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E8PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A8NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A8STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E8KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A8BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E8DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A8DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 E9FCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 E9STNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 E9PRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 A9NAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 A9STFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 E9KNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 A9BEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 E9DATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 A9DATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EAFCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EASTNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EAPRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 AANAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 AASTFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 EAKNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 AABEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EADATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AADATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EBFCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBSTNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EBPRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ABNAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 ABSTFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 EBKNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ABBEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EBDATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ABDATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ECFCO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 ECSTNRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 ECPRZO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ACNAMEO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 ACSTFO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 ECKNRO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ACBEZO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 ECDATVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ACDATBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AINFOO                         PIC X(40).

 


