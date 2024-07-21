      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0012.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0012.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:22   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0012                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE DRPA0012 ZUM ZUORDNUNGSPROGRAMM              *
      *    VERFAHRENSNAME --> PROGRAMMNAME FACHFUNKTION              *
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
       01  PI-DRPA0012.
           05  PI-DRPA0012-FKT                PIC  X(08).
               88  PI-DRPA0012-AUFRUF              VALUE 'DRPA0012'.
           05  PI-DRPA0012-DATEN.
               10  PI-DRPA0012-FUNKTIONSWEISE PIC  9(02).
                   88  PI-DRPA0012-FORGET          VALUE 01.
                   88  PI-DRPA0012-KEEP            VALUE 02.
               10  PI-DRPA0012-VERFNAME       PIC  X(12).
               10  PI-DRPA0012-PRODGB         PIC  9(04).
               10  PI-DRPA0012-PRODSCHL       PIC  9(04).
               10  PI-DRPA0012-STICHTAG       PIC  X(10).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0012.
           05  PO-DRPA0012-RC                 PIC  X(03).
               88  PO-DRPA0012-RC-OK                 VALUE 'OK '.
               88  PO-DRPA0012-RC-NOT-FOUND          VALUE 'NFD'.
               88  PO-DRPA0012-RC-NOK                VALUE 'NOK'.
           05  PO-DRPA0012-DATEN.
               10  PO-DRPA0012-PROGRAMMNAME   PIC  X(08).
               10  PO-DRPA0012-FUNKTIONSNAME  PIC  X(08).
      *
      ******** ENDE COPY-MEMBER (DRPA0012) *******************
