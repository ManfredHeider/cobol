      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:32:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0023      TRACE FUELLEN (ALTANW.)         * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    04.05.1993                                       * 00070000
      *    STAND:    04.05.1993                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *    FELDER FUER TRACE IN ALTANWENDUNGEN                          00110000
      *    (ZUGEHÖRIGES COPY-ELEMENT DER PROCEDURE DIV.: DPAT0023)      00110100
      *                                                                 00111000
      *    SCHNITTSTELLE: VGL. DPAT0023                                 00120000
      *                                                                 00124000
      *                                                                 00142000
      *    Z-DWAT0023-TRC-GES: GESAMTER TRACE !!!                       00143000
      *                        --> FUER AUSGABEN                        00144000
                                                                        00144100
       01  FILLER                             PIC  X(016)               00144200
                                              VALUE '>>>TRACE-START>>'. 00144300
       01  Z-DWAT0023-TRC-GES.                                          00146000
                                                                        00146100
      *    *  TRACE-LABEL BEREICH                                       00146200
           05  Z-DWAT0023-TRCL.                                         00146300
               10  FILLER                     PIC  X(010) VALUE         00147000
                                                         '>TRCL>>>>>' . 00148000
               10  Z-DWAT0023-TRCL-VORN       PIC  X(770) VALUE SPACE.  00149000
               10  Z-DWAT0023-TRCL-NEU        PIC  X(010) VALUE SPACE.  00149100
               10  FILLER                     PIC  X(010) VALUE         00149200
                                                          '<<<<<TRCL<'. 00149300
           05  FILLER REDEFINES Z-DWAT0023-TRCL.                        00149400
               10  FILLER                     PIC  X(010).              00149500
               10  FILLER                     PIC  X(010).              00149600
               10  Z-DWAT0023-TRCL-HINTEN     PIC  X(770).              00149700
               10  FILLER                     PIC  X(010).              00149800
                                                                        00150500
      *    *  TRACE-VAR BEREICH                                         00150600
           05  Z-DWAT0023-TRCV.                                         00150700
               10  Z-DWAT0023-TRCV-VORN       PIC X(1040) VALUE SPACE.  00151000
               10  Z-DWAT0023-TRCV-NEU        PIC X(0080) VALUE SPACE.  00151100
           05  FILLER REDEFINES Z-DWAT0023-TRCV.                        00151900
               10  FILLER                     PIC X(0080).              00152100
               10  Z-DWAT0023-TRCV-HINTEN     PIC X(1040).              00152200
                                                                        00152400
       01  FILLER                             PIC  X(016)               00152500
                                              VALUE '<<<TRACE-ENDE<<<'. 00152600
      *                                                                 00152700
       01  Z-DWAT0023-ARBEITSFELDER.                                    00152800
                                                                        00152900
      *  ---> EINGABE: AKTUELL ZU TRACENDER LABEL                       00153000
           05  Z-DWAT0023-TRC-LABEL           PIC  X(010).              00153100
      *       ZWISCHENFELD FUER TRACE-LABEL                             00153200
           05  Z-DWAT0023-TRCL-ZWI            PIC  X(770).              00153300
                                                                        00153400
      *  ---> EINGABE: AKTUELL ZU TRACENDER VAR-BEREICH                 00153500
           05  Z-DWAT0023-TRC-V80             PIC X(0080).              00153600
      *       ZWISCHENFELD FUER TRACE-VAR                               00153700
           05  Z-DWAT0023-TRCV-ZWI            PIC X(1040).              00153800
                                                                        00153900
      *       FELD FUER RESP-OPTION BEI SEND-BEFEHL                     00154000
           05  Z-DWAT0023-RESP                PIC S9(004) COMP.         00154100
                                                                        00154200
       01  K-DWAT0023-KONSTANTEN.                                       00154300
                                                                        00154400
      *       ZWISCHENFELD FUER TRACE                                   00154500
           05  K-DWAT0023-TRC-LEN             PIC S9(004) COMP          00154600
                                                          VALUE 1920.   00154700
      *                                                                 00190000
      ******* ENDE COPY-MEMBER DWAT0023 (TRACE - ALTANW.)     ********* 00200000
