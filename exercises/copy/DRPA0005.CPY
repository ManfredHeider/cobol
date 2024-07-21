      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:44:46   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0005                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE ZUM LESE-OZP STEUERUNG_DTA                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. MUELLER                                       *
      *    DATUM:    16.01.1996                                       *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    ! NEU                       ! MUELLER  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *   - ANHAND DER UEBERGEBENEN GESELLSCHAFTSNUMMER WERDEN
      *     AUS TABLE STEUERUNG_DTA DIE DATENFELDER FREIGABE, ANZAHL-
      *     FEHLER, PROZENTSATZ UND LAST-UPDATE ERMITTELT
      *   - PI-FELDER:  - FUNKTIONSAUFRUF
      *                 - GESELLSCHAFTSNUMMER
      *   - PO-FELDER:  - RETURNCODE (OK, NFD ODER NOK)
      *                 - DATENFELDER FUER DIALOGOBERFLAECHE PA01
      *                 - LAST-UPDATE AUS TABELLE STEUERUNG_DTA
      *                   ALS KENNUNG FUER UPDATE
      *
      ****************************************************************

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0005.
           05  PI-DRPA0005-FKT                    PIC  X(08).
               88 PI-DRPA0005-AUFRUF              VALUE 'DRPA0005'.
           05  PI-DRPA0005-DATEN.
               10 PI-DRPA0005-P-ID-INTERN-PG      PIC  9(04).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0005.
           05  PO-DRPA0005-RC                     PIC  X(03).
               88 PO-DRPA0005-RC-OK                    VALUE 'OK '.
               88 PO-DRPA0005-RC-NOT-FOUND             VALUE 'NFD'.
               88 PO-DRPA0005-RC-NOK                   VALUE 'NOK'.
           05  PO-DRPA0005-DATEN.
               10 PO-DRPA0005-FREIGABE            PIC  X(01).
               10 PO-DRPA0005-ANZAHL-FEHLER       PIC S9(07).
               10 PO-DRPA0005-PROZ-SATZ-FEHLER    PIC  9(03).
               10 PO-DRPA0005-LAST-UPDATE         PIC  X(26).
      *
      ******** ENDE COPY-MEMBER (DRPA0005) *******************

