      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1100.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki1100.cpv  $
      *
      *   Rev 2.1   20 Jul 2000 15:04:04   T13EX40
      *AKI00101  neue VB-Schnittstelle Direktionsübergreifend
      *
      *   Rev 2.0   Feb 09 2000 15:20:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  PI-DRKI1100.
           05  PI-DRKI1100-TAB-CNT           PIC S9(9) COMP.
           05  PI-DRKI1100-TAB            OCCURS 999.
               10 PI-DRKI1100-VBNUMMER       PIC S9(9) COMP.
               10 PI-DRKI1100-DIR-ALT        PIC S9(4) COMP.
               10 PI-DRKI1100-DIR-NEU        PIC S9(4) COMP.

      *
       01  PO-DRKI1100.
           05  PO-DRKI1100-RC                 PIC  X(003).
               88 PO-DRKI1100-RC-OK                      VALUE 'OK '.
               88 PO-DRKI1100-RC-NOK                     VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************
