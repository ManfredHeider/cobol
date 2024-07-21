      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0002.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0002.CPv  $
      *
      *   Rev 2.4   Jan 07 2021 11:48:06   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.3   Sep 30 2011 09:01:42   t13ae13
      *AVB01185: Maßnahmenpaket - Teil 1
      *
      *   Rev 2.2   Jun 23 2003 11:52:02   t13ae13
      *FVB00526: Fehlerbehebung aus Dim 7ukunft
      *
      *   Rev 2.1   Jan 14 2003 09:22:26   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.0   Feb 27 1999 15:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1997 12:15:34   T13AE26
      *AVB00398 - 40Proz.Klausel Teil2
      *
      *   Rev 1.0   Nov 04 1995 09:03:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0002                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN DIALOGEN (CADD-I-KOMM-VAR, CADD-O-KOMM-VAR)       *
      *    FÜR XA02                                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    04.05.1993                                       *
      *****************************************************************
      *    VERSION   1    23.05.95 17:16:53
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !04.05.93! 01 ! NEU                       ! GIESE    *
      *         !16.09.93! 02 ! ERWEITERUNG ÜBERGABE      ! GIESE    *
      * FEVB0240!28.04.95!    ! KOMMENTAR                 ! GERLACH  *
      * AFVB0207!10.05.95! 03 ! KOMMENTAR                 ! GERLACH  *
      * AFVB0303!22.07.97!    ! UMBAU: INPUT- UND OUTPUT- ! SCHÖLLH. *
      *         !        !    !  SCHNITTSTELLE ZU XA02    !          *
      *         !        !    !  (DRVB0047 ENTFÄLLT)      !          *
      * AVB00398!25.11.97! 04 ! neue Auftragsnummer       ! T. Moll  *
      * AVB00649!25.11.02!2.1 ! neues Aufstiegssystem     ! S. Walb  *
      * FVB00526!14.04.03!2.2 ! Erweiterung Übergabe      ! S. Walb  *
      * AVB01185!06.09.11!2.3 ! neues Aufstiegssystem 2011! S. Walb  *
191120* VLA-532 !19.11.20!2.4 ! NEUES AUFSTIEGSSYSTEM 2021! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   PI-DRVB0002-HOEHER-STUFE     J/N
      *       SOLL DIE STUFE GEPRÜFT WERDEN?
      *
      *   PI-DRVB0002-HOEHER-VBPRAXIS J/N
      *       SOLL DIE VBPRAXIS GEPRÜFT WERDEN?
      *
      *   PI-DRVB0002-LAST-STF-WECHSEL
      *   PI-DRVB0002-LAST-VBP-WECHSEL
      *       DAS DATUM WIRD MASKENGERECHT IM EUROFORMAT ÜBERTRAGEN
      *
      **************************************************************

220796 01  PI-DRVB0002.
220796     05  PI-DRVB0002-CADD-I-KOMM-VAR            PIC  X(128).
220796     05  FILLER REDEFINES   PI-DRVB0002-CADD-I-KOMM-VAR.
220796*
220796         10  PI-DRVB0002-FUNKTION               PIC  X(008).
220796             88  PI-DRVB0002-BEFOERD-PRF-XA02   VALUE 'DRVB0002'.
220796
220796         10  PI-DRVB0002-HOEHER-STUFE           PIC  X(001).
220796         10  PI-DRVB0002-HOEHER-VBPRAXIS        PIC  X(001).
220796         10  PI-DRVB0002-MS-BEZ-KURZ            PIC  X(004).
220796         10  PI-DRVB0002-LAST-STF-WECHSEL       PIC  X(010).
220796         10  PI-DRVB0002-LAST-VBP-WECHSEL       PIC  X(010).
220796         10  PI-DRVB0002-MS-NR                  PIC S9(004) COMP.
220796         10  PI-DRVB0002-BS-NR                  PIC S9(004) COMP.
220796         10  PI-DRVB0002-VBP-NR                 PIC S9(004) COMP.
251102         10  PI-DRVB0002-KNR-GESELL-TYP         PIC S9(004) COMP.
251102         10  PI-DRVB0002-AUFSTIEG               PIC  X(01).
251102             88  PI-DRVB0002-AUFSTIEG-NEU            VALUE '1'.
251102             88  PI-DRVB0002-AUFSTIEG-ALT-NEU        VALUE '2'.
251102             88  PI-DRVB0002-AUFSTIEG-ALT            VALUE '3'.
060911             88  PI-DRVB0002-AUFSTIEG-2011           VALUE '4'.
060911             88  PI-DRVB0002-AUFSTIEG-2007-2011      VALUE '5'.
060911             88  PI-DRVB0002-AUFSTIEG-ALT-07-11      VALUE '6'.
191120             88  PI-DRVB0002-AUFSTIEG-2021           VALUE '7'.
140403         10  PI-DRVB0002-ALP-VBAK-ALD           PIC  9(001).
191120**** STEUERT IM UNTERDIALOG, OB AUF STUFE ODER PRAXIS GEPRÜFT
191120**** WERDEN SOLL (HOEHER-STUFE/-VBPRAXIS KÖNNEN BEIDE J SEIN)
191120**** IST NUR PGVB0073 RELEVANT
191120         10  PI-DRVB0002-PRF                    PIC  X(01).
191120             88  PI-DRVB0002-PRF-STUFE          VALUE 'S'.
191120             88  PI-DRVB0002-PRF-PRAXIS         VALUE 'P'.
191120**** STEUERT IM UNTERDIALOG, OB WEITERBLÄTTERN MÖGLICH IST
191120         10  PI-DRVB0002-WEITER                 PIC  X(01).
191120             88  PI-DRVB0002-WEITER-JA          VALUE 'J'.
191120             88  PI-DRVB0002-WEITER-NEIN        VALUE 'N'.
191120**** STEUERT IM UNTERDIALOG, OB ZURÜCKBLÄTTERN MÖGLICH IST
191120         10  PI-DRVB0002-ZURUECK                PIC  X(01).
191120             88  PI-DRVB0002-ZURUECK-JA         VALUE 'J'.
191120             88  PI-DRVB0002-ZURUECK-NEIN       VALUE 'N'.

220796*        15 Z-DRVB0002-HOEHER-STUFE     PIC  X(01).
220796*        15 Z-DRVB0002-HOEHER-VBPRAXIS  PIC  X(01).
220796*        15 Z-DRVB0002-MS-BEZ-KURZ      PIC  X(04).
220796*        15 Z-DRVB0002-LAST-STF-WECHSEL PIC  X(10).
220796*        15 Z-DRVB0002-LAST-VBP-WECHSEL PIC  X(10).
220796*        15 Z-DRVB0002-MS-NR            PIC S9(04) COMP.
220796*        15 Z-DRVB0002-BS-NR            PIC S9(04) COMP.
220796*        15 Z-DRVB0002-VBP-NR           PIC S9(04) COMP.
220796*        15 Z-DRVB0002-DIR-NR           PIC S9(04) COMP.
      *
220796 01  PO-DRVB0002.
220796     05  PO-DRVB0002-CADD-O-KOMM-VAR            PIC  X(128).
220796     05  FILLER REDEFINES   PO-DRVB0002-CADD-O-KOMM-VAR.
220796*
220796         10  PO-DRVB0002-PRFERG-STF-EH          PIC  X(001).
220796             88  PO-DRVB0002-PRFERG-STF-EH-OK   VALUE '1'.
220796             88  PO-DRVB0002-PRFERG-STF-EH-NOK  VALUE '2'.
220796         10  PO-DRVB0002-PRFERG-STF-SEM         PIC  X(001).
220796             88  PO-DRVB0002-PRFERG-STF-SEM-OK  VALUE '1'.
220796             88  PO-DRVB0002-PRFERG-STF-SEM-NOK VALUE '2'.
220796         10  PO-DRVB0002-PRFERG-VBP-EH          PIC  X(001).
220796             88  PO-DRVB0002-PRFERG-VBP-EH-OK   VALUE '1'.
220796             88  PO-DRVB0002-PRFERG-VBP-EH-NOK  VALUE '2'.
220796         10  PO-DRVB0002-PRFERG-VBP-SEM         PIC  X(001).
220796             88  PO-DRVB0002-PRFERG-VBP-SEM-OK  VALUE '1'.
220796             88  PO-DRVB0002-PRFERG-VBP-SEM-NOK VALUE '2'.
251102         10  PO-DRVB0002-PRFERG-STF-NEU-EH          PIC  X(001).
251102             88  PO-DRVB0002-PRFERG-ST-N-EH-OK  VALUE '1'.
251102             88  PO-DRVB0002-PRFERG-ST-N-EH-NOK VALUE '2'.
251102         10  PO-DRVB0002-PRFERG-STF-NEU-SEM         PIC  X(001).
251102             88  PO-DRVB0002-PRFERG-ST-N-SM-OK  VALUE '1'.
251102             88  PO-DRVB0002-PRFERG-ST-N-SM-NOK VALUE '2'.
060911         10  PO-DRVB0002-PRFERG-STF-11-EH           PIC  X(001).
060911             88  PO-DRVB0002-PRFERG-ST-11-E-OK  VALUE '1'.
060911             88  PO-DRVB0002-PRFERG-ST-11-E-NOK VALUE '2'.
060911         10  PO-DRVB0002-PRFERG-STF-11-SEM          PIC  X(001).
060911             88  PO-DRVB0002-PRFERG-ST-11-S-OK  VALUE '1'.
060911             88  PO-DRVB0002-PRFERG-ST-11-S-NOK VALUE '2'.
191120         10  PO-DRVB0002-PRFERG-STF-21-EH           PIC  X(001).
191120             88  PO-DRVB0002-PRFERG-ST-21-E-OK  VALUE '1'.
191120             88  PO-DRVB0002-PRFERG-ST-21-E-NOK VALUE '2'.
191120         10  PO-DRVB0002-PRFERG-STF-21-SEM          PIC  X(001).
191120             88  PO-DRVB0002-PRFERG-ST-21-S-OK  VALUE '1'.
191120             88  PO-DRVB0002-PRFERG-ST-21-S-NOK VALUE '2'.
191120**** STEUERT IN PGVB0012 DIE WEITEREN ANZEIGEN
191120**** ENTER = WEITERBLÄTTERN AUF DIE NÄCHSTEN BEFBED
191220****   PF3 = ENDE DER ANZEIGE (= VERBLEIB IM PGVB0012)
191120****   PF5 = ZURÜCKBLÄTTERN AUF DIE VORHERIGEN BEFBED
191120         10  PO-DRVB0002-TASTE                  PIC  X(01).
191120             88  PO-DRVB0002-TASTE-ENTER        VALUE 'E'.
191120             88  PO-DRVB0002-TASTE-PF3          VALUE '3'.
191120             88  PO-DRVB0002-TASTE-PF5          VALUE '5'.
      *
      ***************** ENDE DRVB0002 *********************************

