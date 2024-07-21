      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\SO\DPAT0006.CPv  $
      *
      *   Rev 2.1   Apr 14 1999 16:42:34   P13SP99
      *Source gibt es in Host-Produktion, aber
      *kein Archiv mehr dazu.
      *
      *   Rev 2.0   Apr 14 1999 16:27:34   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0006          CICS-FEHLER-BEHANDLUNG      *
      *    --------------------------------------------------------   *
      *    VERSION   001                                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    24.02.1993                                       *
      *    STAND:    25.03.1993                                       *
      *****************************************************************
      *
      *    CICS-FEHLER-ROUTINE, DIE BEI CICS-FEHLER IM HAUPTPROGRAMM
      *    BENOETIGT WIRD
      *
      *------------------------------------------------------------*
      * ZR92-CICS-FEHLER                                           *
      *------------------------------------------------------------*
      * INPUT : C-CICS-RC, ERR-ORT,                                *
      *         EVTL. ERR-VAR                                      *
      *                                                            *
      * OUTPUT: - (KEINE RUECKKEHR ZUM AUFRUFER)                   *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *
      *   - AUFRUF VON UR99-SCHWERER-FEHLER                        *
      *                                                            *
      *------------------------------------------------------------*
       ZR92-CICS-FEHLER SECTION.
       ZR92-ANF.
      *
      *    * VERSORGEN SCHNITTSTELLE
           SET ERR-FTYP-CICS               TO TRUE
           MOVE C-CICS-RC                  TO ERR-CICS-RC
           PERFORM UR99-SCHWERER-FEHLER
           .
       ZR92-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER DPAT0006 (CICS-FEHLER-BEHANDLUNG) ****
CCC009*    EJECT
