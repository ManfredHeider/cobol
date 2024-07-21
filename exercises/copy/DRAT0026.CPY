      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0026.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\DRAT0026.CPv  $
      *
      *   Rev 2.4   20 Jun 2001 11:03:30   t13ae34
      *ACV00015-CTV-Druck; hier: CTV-Meldungen
      *
      *   Rev 2.3   10 May 2001 13:09:50   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.2   Apr 12 1999 12:02:14   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   REV 2.0   MAR 01 1999 14:44:42   T13AE34
      *NT-UMSTELLUNG
      *
      *   Rev 1.3   27 AUG 1997 17:42:22   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.2   MAY 12 1997 11:00:20   T13EX11
      *COPIES FUER AUFRUF UPAT0013 EINGEFUEGT
      *
      *   Rev 1.1   MAR 20 1997 16:56:54   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   NOV 04 1995 08:38:42   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    COPY-MEMBER: RAHMEN-COPIES AUFRUFPARAMETER  (DRAT0026)     *
      *    --------------------------------------------------------   *
      *    VERSION                                                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    05.03.1993                                       *
      *    STAND:    25.03.1993                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      *         !05.03.93! 01 ! NEUERSTELLUNG             ! SCHÖLLH.  *
      * AFPA0002!03.02.97! 02 ! COPYS FüR AUFRUF ZWPA0015 ! AUERBACH  *
      *         !        !    ! EINGEFüGT                 !           *
      * AFVB0339!16.04.97! 03 ! COPYS FÜR AUFRUF UPAT0013 ! SCHÖLLH.  *
      *         !        !    ! EINGEFÜGT                 !           *
      * afpa0022!22.08.97!    ! call upat0018 uebernimt   ! opritz    *
      *         !        !    ! zwpa0015 und zwat0008     !           *
      * AAT00005!17.02.99!1.4 ! EURO-SWITCH               ! HEIDER    *
      * ACV00015!09.03.01!2.3 ! CTV AUS NEUDIALOGEN       ! HEIDER    *
      * ACV00015!20.06.01!2.4 ! MIT FKT. DRAT0344         ! HEIDER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    UEBERGABEBEREICH UPAT0001 (AUFBEREITEN ERR-ERROR-BEREICH)
           COPY DRAT0032.
      *
      *    UEBERGABEBEREICH UPAT0003 (ERMITTELN HINWEIS/FTASTEN-ZEILE)
           COPY DRAT0020.
      *
      *    UEBERGABEBEREICH UPAT0005 (SQL-ERROR-INFOS IN DB2 SCHREIBEN)
           COPY DRAT0025.
      *
160497*    UEBERGABEBEREICH UPAT0013 (PROTOKOLLIEREN DIALOGZUGRIFFE)
160497     COPY DRAT0129.
160497     COPY DRAT0130.
      *
220897*    UEBERGABEBEREICH UPAT0018 (ermitteln vstat und kty-knr)
220897     COPY DRAT0148.
220897     COPY DRAT0149.
220897     COPY DRAT0150.
      *
170299*    UEBERGABEBEREICH UPAT0023 (EURO-MODUL)
170299     COPY DRAT0256.
170299*    FUNKTION 'EURO-SWITCH UMRECHNEN BETRäGE IN MAP'
170299     COPY DRAT0283.
170299*    FUNKTION 'EURO-SWITCH AKTIV'
170299     COPY DRAT0291.
      *
030297*    UEBERGABEBEREICH ZWPA0015 (ERMITTELN VSTATUS-TEXT)
030297     COPY DRPA0151.
030297     COPY DRPA0152.
      *
090301*    UEBERGABEBEREICH ZWAT0008 (KTY-OZP)
090301     COPY DRAT0138.
200601*    FUNKTION 'ERMITTELN BEZ ZU KTY-NR, KNR-BEZ-10
200601     COPY DRAT0344.
090301*    FUNKTION 'BEANTWORTEN FRAGE: EXISTIERT ZU KTY, BEZ-KURZ
090301*              MINDESTENS EIN EINTRAG IN KTY?'
090301     COPY DRAT0353.
      *
      ******** ENDE COPY-MEMBER RAHMEN-COPIES AUFRUFPARMS (DRAT0026) **
