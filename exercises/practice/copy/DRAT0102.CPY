      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0102.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0102.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   Apr 30 1996 14:34:36   T13EX11
      *AFVB0226  KNR-SCHECKSPERRE neu
      *
      *   Rev 1.2   Mar 26 1996 17:50:08   T13EX16
      *AFVB0226 Copy neu verwendet alter Inhalt
      *         überflüssig
      *
      *   Rev 1.1   30 Jan 1996 11:51:56   P13SP99
      *PVCS-Keywords angepaßt
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0102   KNR-KONST. KTY-63 GRUPPENKLASSEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    12.03.1996                                       *
      *****************************************************************
      *    VERSION   1    12.03.96 15:03:48
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0244!12.12.95! 01 ! NEU                       ! SCHINDLER*
      * AFVB0244!12.03.96! 01 ! NEUER INHALT,DER ALTE WIRD! AUERBACH *
      *         !        !    ! NICHT MEHR BENÖTIGT       !          *
      * AFVB0244!30.04.96! 02 ! KNR-SCHECKSPERRE NEU      ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE KTY-63   GRUPPENKLASSEN
      *
       01  FILLER.
           05  K-KNR-MAST-ZUSAETZE            PIC S9(004) COMP VALUE  1.
           05  K-KNR-SONST-MONETAERE-AUSZ     PIC S9(004) COMP VALUE  2.
300496     05  K-KNR-SCHECKSPERRE             PIC S9(004) COMP VALUE  3.
      *
      ***** ENDE COPY-MEMBER DRAT0102  KNR-KONST. GRUPPENKLASSEN  .*****
