      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:14:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010001
      *    COPY-MEMBER: DPAT0021      UPRO: CICS-FEHLER-BEHANDLUNG    * 00020001
      *    --------------------------------------------------------   * 00030001
      *    VERSION   001                                              * 00040001
      *    --------------------------------------------------------   * 00050001
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060001
      *    DATUM:    03.04.1993                                       * 00070001
      *    STAND:    21.04.1993                                       * 00080003
      ***************************************************************** 00090001
      *                                                                 00100001
      *    FEHLERBEHANDLUNGS-ROUTINE, DIE IN EINEM UPRO BEI AUFTRETEN   00110001
      *    EINES CICS-FEHLERS (INCL. CICS-DATEI-ZUGRIFFE) BENOETIGT WIRD00120001
      *                                                                 00130001
      *------------------------------------------------------------*    00140001
      * XR92-UPRO-CICS-FEHLER                                      *    00150001
      *------------------------------------------------------------*    00160001
      * INPUT : C-CICS-RC, ERR-ORT, K-PGM-NAME                     *    00170001
      *                                                            *    00180001
      * OUTPUT: GEFUELLTER ERR-ERROR-BEREICH                       *    00190001
      *                                                            *    00200001
      * VERARBEITUNG:                                              *    00210001
      *   - FUELLEN DER ERROR-SCHNITTSTELLE FUER UPRO-CICS-FEHLER  *    00220001
      *                                                            *    00230001
      *------------------------------------------------------------*    00240001
       XR92-UPRO-CICS-FEHLER SECTION.                                   00250001
       XR92-ANF.                                                        00260001
      *                                                                 00270001
      *    * VERSORGEN SCHNITTSTELLE                                    00280001
           SET ERR-RC-SCHWERER-FEHLER      TO TRUE                      00280103
           SET ERR-FTYP-CICS               TO TRUE                      00280202
           MOVE K-PGM-NAME                 TO ERR-UPRONAME              00280301
           MOVE C-CICS-RC                  TO ERR-CICS-RC               00280401
           .                                                            00283901
       XR92-EXIT.                                                       00284001
           EXIT.                                                        00284101
      *                                                                 00284201
      ******* ENDE COPY-MEMBER DPAT0021 (UPRO: CICS-FEHLER)  ********   00284301
CCC009*    EJECT                                                        00285001
