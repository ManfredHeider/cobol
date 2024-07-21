      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU011S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU011S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:44:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
       01  UU011S.
      *
      ******************************************************************
      *    STRUKTURSATZ
      ******************************************************************
      *
           10  SSA                     PIC 99.
           10  SKEY.
               20  SMANR              PIC 9(7)  COMP-3.
               20  SSTUFE             PIC 999   COMP-3.
               20  SUMANR             PIC 9(7)  COMP-3.
               20  SUBISDAT           PIC 9(6)  COMP-3.
      *
      *
      *
           10  SUVONDAT               PIC 9(6)  COMP-3.
           10  SLBDAT                 PIC 9(6)  COMP-3.
           10  SFILLER                PIC 9(10).
      *
      ******************************************************************
      *    STRUKTURSATZ.
      ******************************************************************
      *
      *    NOTE *** LAENGE 34 BYTES
      *           * IM VORSATZ ----->
      *           * SUVONDAT = LETZTES LADEDATUM
      *           * SLBDAT   = LETZTES ENTLADEDATUM
      *           * SFILLER  = SATZANZAHL
      *           ***.
      *
      *
      *

