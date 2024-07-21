      * T13EX39   Stand: 21/07/98    21:34:52
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0380.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0380.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 Feb 1999 12:10:12   T13EX40
      *AKI00016 - Funktionserwseiterung
      *
      *   Rev 1.1   28 Jan 1999 10:10:56   T13EX39
      *afki0001 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 17:03:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0380.
           05  PI-DRKI0380-DATEN.
               10 PI-DRKI0380-AKTION                PIC X.
                  88 PI-DRKI0380-AKTION-INS             VALUE 'I'.
                  88 PI-DRKI0380-AKTION-UPD             VALUE 'U'.
                  88 PI-DRKI0380-AKTION-DEL             VALUE 'D'.
               10 PI-DRKI0380-PTRCHECK                 PIC X.
                  88 PI-DRKI0380-CHECK-ON               VALUE 'Y'.
                  88 PI-DRKI0380-CHECK-OFF              VALUE 'N'.
               10 PI-DRKI0380-PTRZIEL               PIC S9(9) COMP.
               10 PI-DRKI0380-PTRCLASSNAME.
                  49 PI-DRKI0380-PTRCLASSNAME-LEN   PIC S9(4) COMP.
                  49 PI-DRKI0380-PTRCLASSNAME-TEXT  PIC X(40).
               10 PI-DRKI0380-PTRTABLENAME.
                  49 PI-DRKI0380-PTRTABLENAME-LEN   PIC S9(4) COMP.
                  49 PI-DRKI0380-PTRTABLENAME-TEXT  PIC X(40).
               10 PI-DRKI0380-PTRKEYDATA.
                  49 PI-DRKI0380-PTRKEYDATA-LEN     PIC S9(4) COMP.
                  49 PI-DRKI0380-PTRKEYDATA-TEXT    PIC X(200).
               10 PI-DRKI0380-PTRSQLDATA.
                  49 PI-DRKI0380-PTRSQLDATA-LEN     PIC S9(4) COMP.
                  49 PI-DRKI0380-PTRSQLDATA-TEXT    PIC X(3600).
      *
       01  PO-DRKI0380.
           05  PO-DRKI0380-RC                       PIC  X(003).
               88 PO-DRKI0380-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0380-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
