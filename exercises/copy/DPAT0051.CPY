      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0051.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0051.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   May 12 1997 11:01:58   T13EX11
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0051       DIALOG-RAHMENSTEUERUNG         *
      *     DAS COPY ENTHÄLT DEN AUFRUF AUF UPAT0013, DAS DIE         *
      *     DIALOGZUGRIFFE FÜR BEST. TRANSAKTIONEN PROTOKOLLIERT      *
      *     BEIM AUFRUF WIRD U.A. DIE VB-NR ÜBERGEBEN                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER                                  *
      *    DATUM:    16.04.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0339!16.04.97!    ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *------------------------------------------------------------*
       UR10-PROTOKOLL-MIT-VBNR SECTION.
       UR10-ANF.
      *------------------------------------------------------------*
      * INPUT : CADD-OB-VB-NR,                                     *
      *         K-UPAT0013, CARU-STANDARD-STATUS-BEREICH           *
      * OUTPUT: PO-DRAT0130-RC                                     *
      * VERARBEITUNG:                                              *
      *   - VERSORGEN SCHNITTSTELLE (U.A. VB-NR)                   *
      *   - AUFRUF UPAT0013: PROTOKOLLIERUNG DIALOG-ZUGRIFF        *
      *        (MIT FUNKTION DRAT0130: PROTOKOLL MIT VB-NR)        *
      *------------------------------------------------------------*
      *
           INITIALIZE                         PI-DRAT0130
                                              PO-DRAT0130
           MOVE CADD-OB-VB-NR                 TO PI-DRAT0130-ST-NR
           SET  PI-UPAT0013-DRAT0130          TO TRUE
      *
CCCPY1*    CALL K-UPAT0013 USING BY REFERENCE DFHEIBLK
CCC004*                                       DFHCOMMAREA
CCC004*                                       PI-UPAT0013
CCC004*                                       PI-DRAT0130
CCC004*                                       PO-DRAT0130
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM UR99-SCHWERER-FEHLER
           END-IF
           .
       UR10-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER DPAT0051 (DIALOG-RAHMENSTEUERUNG) ******
