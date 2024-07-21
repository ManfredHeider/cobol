      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:44:50   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0002                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE DRPA0006 FUNKTIONSAUFRUF ZUM ERMITTELN       *
      *    DER AKTUELLEN SPERRKRITERIEN IN DIALOG PA01                *
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
      *   - FUNKTION: SCHNITTSTELLE AUS DIALOG PA01 NACH EINGABE
      *     DER ZU BEARBEITENDEN GESELLSCHAFTSNUMMER ZUM DATENCONTAINER
      *     VON TRANSAKTION PA01
      *   - PI-DATEN: - FUNKTIONSAUFRUF
      *               - GESELLSCHAFTSNUMMER
      *   - PO-DATEN: - RETURNCODE
      *               - DATENFELDER FUER DIALOGOBERFLAECHE:
      *                 FREIGABE, ANZAHL-FEHLER UND PROZENTSATZ
      *               - CONTAINER-CA MIT INHALT LAST-UPDATE
      *
      ****************************************************************

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0006.
           05  PI-DRPA0006-FKT                PIC  X(08).
               88  PI-DRPA0006-AUFRUF              VALUE 'DRPA0006'.
           05  PI-DRPA0006-DATEN.
               10  PI-DRPA0006-PRODGB         PIC  9(04).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0006.
           05  PO-DRPA0006-RC                 PIC  X(03).
               88  PO-DRPA0006-RC-OK                 VALUE 'OK '.
               88  PO-DRPA0006-RC-NOT-FOUND          VALUE 'NFD'.
               88  PO-DRPA0006-RC-NOK                VALUE 'NOK'.
           05 PO-DRPA0006-DATEN.
               10  PO-DRPA0006-SPERKZ         PIC  X(01).
               10  PO-DRPA0006-FEHLZA         PIC S9(07).
               10  PO-DRPA0006-PROZNT         PIC  9(03).
               10  PO-DRPA0006-MELDUNG-TYP    PIC  X(01).
               10  PO-DRPA0006-MELDUNG-NR     PIC  9(04).
           05 PO-DRPA0006-CONTAINER-CA.
               10  PO-DRPA0006-LAST-UPDATE    PIC  X(26).
      *
      ******** ENDE COPY-MEMBER (DRPA0006) *******************

