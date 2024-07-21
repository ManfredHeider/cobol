      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:10   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0008                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE ZWISCHEN DIALOGSTEUERUNG PA01 UND            *
      *    ZUGEHOERIGEM DATENCONTAINER FUER FUNKTIONSAUFRUF INSERT    *
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
      *   - FUNKTION: SCHNITTSTELLE AUS DIALOG PA01 ZUR INSERT-
      *               VERARBEITUNG
      *   - PI-DATEN: - FUNKTIONSAUFRUF
      *               - BILDSCHIRMEINGABEDATEN:
      *                 PRODUKTGEBER, SPERRKENNZEICHEN, FEHLERZAHL,
      *                 PROZENTSATZ UND OK-JA/NEIN
      *               - USER-KENNUNG FUER INSERT: USER-ID/TERMINAL-ID
      *   - PO-DATEN: - RETURNCODE: - BEI PLAUSIBLEN EINGABEN  'OK'
      *                             - BEI ERFOLGREICHEM INSERT 'INS'
      *                             - BEI UNPLAUSIBLEM SPERRKZ 'SPN'
      *                             - BEI UNPLAUSIBLER ANZAHL  'FZN'
      *                             - BEI UNPLAUSIBLEM PROZENT 'PZN'
      *                             - BEI KONKURIERENDEM ZUGR. 'KZG'
      *                             - BEI SONSTIGEM FEHLER     'NOK'
      *               - DATEN: ./.
      *
      ****************************************************************

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0008.
           05  PI-DRPA0008-FUNKTION           PIC X(08).
               88  PI-DRPA0008-AUFRUF         VALUE 'DRPA0008'.
           05  PI-DRPA0008-DATEN.
               10  PI-DRPA0008-PRODGB         PIC  9(04).
               10  PI-DRPA0008-SPERKZ         PIC  X(01).
               10  PI-DRPA0008-FEHLZA         PIC S9(07).
               10  PI-DRPA0008-PROZNT         PIC  9(03).
               10  PI-DRPA0008-OK             PIC  X(01).
               10  PI-DRPA0008-USER-ID        PIC  X(08).
               10  PI-DRPA0008-TERMINAL-ID    PIC  X(08).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0008.
           05  PO-DRPA0008-RC                 PIC  X(03).
               88  PO-DRPA0008-RC-OK                 VALUE 'OK '.
               88  PO-DRPA0008-RC-INSERT-ERFOLGT     VALUE 'INS'.
               88  PO-DRPA0008-RC-SPERKZ-NOK         VALUE 'SPN'.
               88  PO-DRPA0008-RC-FEHLZA-NOK         VALUE 'FZN'.
               88  PO-DRPA0008-RC-PROZNT-NOK         VALUE 'PZN'.
               88  PO-DRPA0008-RC-KONKUR-ZUGRIFF     VALUE 'KZG'.
               88  PO-DRPA0008-RC-NOK                VALUE 'NOK'.
           05  PO-DRPA0008-DATEN.
               10  PO-DRPA0008-MELDUNG-TYP    PIC  X(01).
               10  PO-DRPA0008-MELDUNG-NR     PIC  9(04).
      *
      ******** ENDE COPY-MEMBER (DRPA0008) *******************

