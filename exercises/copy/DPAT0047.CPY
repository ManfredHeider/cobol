      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0047.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0047.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 27 1996 19:21:32   T13EX16
      *AFVB00226 Zeilennummern gelöscht
      *
      *   Rev 1.0   Mar 27 1996 08:37:52   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      **************************************************************
      *    COPY-MEMBER: DPAT0047  TITEL VERSORGEN - INDIVIDUELL    *
      *    ------------------------------------------------------  *
      *    VERSION   001                                           *
      *    ------------------------------------------------------  *
      *    ERSTELLT: R. AUERBACH                                   *
      *    DATUM:    14.03.1996                                    *
      *    STAND:    14.03.1996                                    *
      **************************************************************
      *
      *    IN SPEZIELLEN DIALOGEN IST EINE INDIVIDUELLE BESTÜCKUNG
      *    DER TITEL ERFORDERLICH. DIESES COPY SCHAFFT EINEN
      *    ANKNÜPFPUNKT FÜR FACHFUNKTIONEN DIE DEN DIALOGTITEL
      *    FREI BESTIMMEN KÖNNEN.
      *
      *------------------------------------------------------------*
      * R711-TITEL-VERSORG                                         *
      *------------------------------------------------------------*
      * INPUT :                                                    *
      *                                                            *
      * OUTPUT:                                                    *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     RUFT EINE DEFINIERTE SECTION AUF                       *
      *                                                            *
      *------------------------------------------------------------*
       R711-TITEL-VERSORG SECTION.
       R711-ANF.
           MOVE 'R711 ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           PERFORM K1-KOPF-TITEL
           .
       R711-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER DPAT0047 (TITEL - INDIVIDUELL) ******
CCC009*    EJECT
