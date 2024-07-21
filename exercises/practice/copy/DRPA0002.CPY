      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:44:34   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00012000
      *    COPY-MEMBER: DRPA0002                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE UPPA0002 FUNKTIONSSTEUERUNG DIALOG PA01      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. MUELLER                                       * 00060000
      *    DATUM:    16.01.1996                                       * 00070000
      ****************************************************************  00091000
      *    VERSION
      ****************************************************************  00091200
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !        !    ! NEU                       ! MUELLER  *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00099600
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *   - ERLÄUTERUNG DER FUNKTIONEN
      *   - WELCHE EINGABEN (PI) ERFORDERLICH (ABH. VON FKT)
      *   - WELCHE AUSGABEN (PO) WERDEN GELIEFERT
      *        (ABH. VON FKT UND RC)
      *   - EVTL. ERLÄUTERUNG ANDERER PI- ODER PO-FELDER
      *
      ****************************************************************  00099600

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPPA0002.
           05  PI-UPPA0002-STEUER             PIC  X(100).
           05                       REDEFINES PI-UPPA0002-STEUER.
               10  PI-UPPA0002-FKT            PIC  X(03).
                   88  PI-PA02-FKT-1                     VALUE 'AM1'.
                   88  PI-PA02-FKT-2                     VALUE 'PRF'.
                   88  PI-PA02-FKT-3                     VALUE 'INS'.
                   88  PI-PA02-FKT-4                     VALUE 'UPD'.
               10  PI-UPPA0002-USER-ID        PIC  X(08).
               10  PI-UPPA0002-TERMINAL-ID    PIC  X(08).
               10  PI-UPPA0002-TIMESTAMP      PIC  X(26).
               10  PI-UPPA0002-LAST-UPDATE    PIC  X(26).
           05 PI-UPPA0002-DATEN               PIC  X(100).
           05                       REDEFINES PI-UPPA0002-DATEN.
               10  PI-UPPA0002-PRODGB         PIC  9(04).
               10  PI-UPPA0002-SPERKZ         PIC  X(01).
               10  PI-UPPA0002-FEHLZA         PIC S9(07).
               10  PI-UPPA0002-PROZNT         PIC  9(03).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPPA0002.
           05  PO-UPPA0002-KONTROLL           PIC  X(50).
           05                       REDEFINES PO-UPPA0002-KONTROLL.
               10  PO-UPPA0002-RC             PIC  X(03).
                   88  PO-PA02-RC-OK                     VALUE 'OK '.
                   88  PO-PA02-RC-NOT-FOUND              VALUE 'NFD'.
                   88  PO-PA02-RC-SPERKZ-NOK             VALUE 'SPN'.
                   88  PO-PA02-RC-FEHLZA-NOK             VALUE 'FZN'.
                   88  PO-PA02-RC-PROZNT-NOK             VALUE 'PZN'.
                   88  PO-PA02-RC-KONKUR-ZUGRIFF         VALUE 'KZG'.
                   88  PO-PA02-RC-NOK                    VALUE 'NOK'.
               10  PO-UPPA0002-LAST-UPDATE    PIC  X(26).
           05 PO-UPPA0002-DATEN               PIC  X(100).
           05                       REDEFINES PO-UPPA0002-DATEN.
               10  PO-UPPA0002-PRODGB         PIC  9(04).
               10  PO-UPPA0002-SPERKZ         PIC  X(01).
               10  PO-UPPA0002-FEHLZA         PIC S9(07).
               10  PO-UPPA0002-PROZNT         PIC  9(03).
      *
      ******** ENDE COPY-MEMBER (DRPA0002) *******************

