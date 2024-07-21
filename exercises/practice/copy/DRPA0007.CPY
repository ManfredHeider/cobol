      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:44:56   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0007                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE ZWISCHEN DIALOGSTEUERUNG PA01 UND            *
      *    ZUGEHOERIGEM DATENCONTAINER FUER FUNKTIONSAUFRUF UPDATE    *
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
      *   - FUNKTION: SCHNITTSTELLE AUS DIALOG PA01 ZUR UPDATE-
      *               VERARBEITUNG
      *   - PI-DATEN: - FUNKTIONSAUFRUF
      *               - BILDSCHIRMEINGABEDATEN:
      *                 PRODUKTGEBER, SPERRKENNZEICHEN, FEHLERZAHL,
      *                 PROZENTSATZ UND OK-JA/NEIN
      *               - USER-KENNUNG FUER UPDATE: USER-ID/TERMINAL-ID
      *               - CONTAINER-CA: LAST-UPDATE
      *   - PO-DATEN: - RETURNCODE: - BEI PLAUSIBLEN EINGABEN  'OK'
      *                             - BEI ERFOLGREICHEM UPDATE 'UPD'
      *                             - BEI UNPLAUSIBLEM SPERRKZ 'SPN'
      *                             - BEI UNPLAUSIBLER ANZAHL  'FZN'
      *                             - BEI UNPLAUSIBLEM PROZENT 'PZN'
      *                             - BEI KONKURIERENDEM ZUGR. 'KZG'
      *                             - BEI SONSTIGEM FEHLER     'NOK'
      *               - DATEN: ./.
      *               - CONTAINER-CA: BEI RC UPD SPACE, ANSONSTEN
      *                 KOPIE AUS PI-CONTAINER-CA
      *
      ****************************************************************

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0007.
           05  PI-DRPA0007-FUNKTION           PIC X(08).
               88  PI-DRPA0007-AUFRUF         VALUE 'DRPA0007'.
           05  PI-DRPA0007-DATEN.
               10  PI-DRPA0007-PRODGB         PIC  9(04).
               10  PI-DRPA0007-SPERKZ         PIC  X(01).
               10  PI-DRPA0007-FEHLZA         PIC S9(07).
               10  PI-DRPA0007-PROZNT         PIC  9(03).
               10  PI-DRPA0007-OK             PIC  X(01).
               10  PI-DRPA0007-USER-ID        PIC  X(08).
               10  PI-DRPA0007-TERMINAL-ID    PIC  X(08).
           05  PI-DRPA0007-CONTAINER-CA.
               10  PI-DRPA0007-LAST-UPDATE    PIC  X(26).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0007.
           05  PO-DRPA0007-RC                 PIC  X(03).
               88  PO-DRPA0007-RC-OK                 VALUE 'OK '.
               88  PO-DRPA0007-RC-UPDATE-ERFOLGT     VALUE 'UPD'.
               88  PO-DRPA0007-RC-SPERKZ-NOK         VALUE 'SPN'.
               88  PO-DRPA0007-RC-FEHLZA-NOK         VALUE 'FZN'.
               88  PO-DRPA0007-RC-PROZNT-NOK         VALUE 'PZN'.
               88  PO-DRPA0007-RC-KONKUR-ZUGRIFF     VALUE 'KZG'.
               88  PO-DRPA0007-RC-NOK                VALUE 'NOK'.
           05  PO-DRPA0007-DATEN.
               10  PO-DRPA0007-MELDUNG-TYP    PIC  X(01).
               10  PO-DRPA0007-MELDUNG-NR     PIC  9(04).
           05  PO-DRPA0007-CONTAINER-CA.
               10  PO-DRPA0007-LAST-UPDATE    PIC  X(26).
      *
      ******** ENDE COPY-MEMBER (DRPA0007) *******************

