      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:13:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0016         ABEND-FEHLER-BEHANDLUNG      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    24.03.1993                                       * 00070000
      *    STAND:    24.03.1993                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *    ABEND-HANDLING FUER ABEND NACH CICS-AUFRUF OHNE RESP-OPTION  00110000
      *                                                                 00850000
      *------------------------------------------------------------*    00860000
      * ZR91-ABEND-RESP                                            *    00870000
      *------------------------------------------------------------*    00880000
      * INPUT : ERR-ORT-SEC                                        *    00890000
      *                                                            *    00900000
      * OUTPUT: -                                                  *    00910000
      *                                                            *    00920000
      * VERARBEITUNG:                                              *    00930000
      *   - DIE SPRUNGMARKE 'ZR91-ANF' WIRD ANGESPRUNGEN, WENN     *    00940000
      *     EIN CICS-FEHLER AUFTRITT UND BEI DEM ENTSPRECHENDEN    *    00950000
      *     CICS-BEFEHL KEINE RESP-OPTION GESETZT WURDE            *    00960000
      *     (= PROGRAMMIERFEHLER)                                  *    00970000
      *   - AUFRUF VON UR93-PROG-FEHLER                            *    00980000
      *                                                            *    00990000
      *------------------------------------------------------------*    01000000
       ZR91-ABEND-RESP SECTION.                                         01010000
       ZR91-ANF.                                                        01020000
           MOVE K-1                     TO ERR-MELD-NR                  01030000
           PERFORM UR93-PROG-FEHLER                                     01040000
           .                                                            01050000
       ZR91-EXIT.                                                       01060000
           EXIT.                                                        01070000
      *                                                                 02350000
      ******* ENDE COPY-MEMBER DPAT0016 (ABEND-FEHLER-BEHANDLUNG) ****  02360000
CCC009*    EJECT                                                        02370000
