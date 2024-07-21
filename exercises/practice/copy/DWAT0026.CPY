      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0026.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0026.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:32:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0026  H-MELDUNGEN MERKEN UND AUSGEBEN     * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    17.05.1993                                       * 00070000
      ***************************************************************** 00090000
      *    VERSION   01   16.09.93                                   *  00091005
      ****************************************************************  00092005
      * AENDERUNGEN:                                                 *  00093005
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00094005
      * -------------------------------------------------------------*  00095005
      *         !16.09.93! 01 ! NEU                       ! SCHÖLLH. *  00096005
      *         !        !    !                           !          *  00097005
      *         !        !    !                           !          *  00098005
      *         !        !    !                           !          *  00099005
      *         !        !    !                           !          *  00099105
      *         !        !    !                           !          *  00099205
      *         !        !    !                           !          *  00099305
      *         !        !    !                           !          *  00099405
      *                                                              *  00099505
      * ENDE-LOGBUCH                                                 *  00099605
      ****************************************************************  00099705
      *                                                                 00100000
      *    SCHNITTSTELLE FUER MERKEN UND AUSGEBEN VON H-MELDUNGEN:      00110000
      *                                                                 00120000
      *    H-MELDUNGEN                                                  00130000
      *    - SIND MELDUNGEN, DIE ERST ANGEZEIGT WERDEN SOLLEN, WENN     00131000
      *      DIE EINGABE FEHLERFREI IST, D.H. KEINE E-MELDUNGEN MEHR    00140000
      *      AUSZUGEBEN SIND                                            00141000
      *    - WERDEN ANGEZEIGT, UND MÜSSEN VOM ANWENDER MIT DATENFREI-   00142000
      *      GABE BESTÄTIGT WERDEN, D.H. DIE ANWENDUNG HÄLT AN UND SETZT00143000
      *      MIT DER WEITEREN BEARBEITUNG ERST FORT, WENN ALLE MELDUNGEN00144000
      *      ANGEZEIGT SIND                                             00145000
      * !!!                                                             00145100
      *    EINSATZ:                                                     00145203
      *    - ZU BEGINN EINES JEDEN DIALOGSCHRITTS AUFRUF VON            00145303
      *      UA1-H-MELDUNGEN-INI (EVTL. IN OP = 'INI')                  00145404
      *    - DIE ROUTINE UA2-H-MELDUNG-INFO MUSS ZU JEDER (!!) H-MELDUNG00146003
      *      AUFGERUFEN WERDEN, UM MITZUTEILEN, OB DIE MELDUNG          00147003
      *      ANZUZEIGEN IST ODER NICHT                                  00148003
      *    - AM ENDE VON 'FI1' IST UA3-H-MELDUNGEN-AUSGEBEN AUFZURUFEN  00149003
      *      UND AUF UA3-ANZEIGE-NOCH/UA3-ANZEIGE-ENDE ABZUFRAGEN       00150003
      *    - BEI UA3-ANZEIGE-NOCH IST 'OP-ABBR' ZU SETZEN (SONST        00160003
      *      ANDERER OP, ABH. VON DIALOG)                               00170003
      *                                                                 00180003
      *    INPUT : NUR BEI UA2                                          00190003
      *    OUTPUT: NUR BEI UA3                                          00200003
      *                                                                 00210003
       01  ZI-DWAT0026-UA2-INPUT.                                       00580002
           05  ZI-DWAT0026-UA2-H-MELDNR       PIC  9(004).              00590002
           05  ZI-DWAT0026-UA2-ANZUZEIG       PIC  9(001).              00600002
               88  UA2-ANZUZEIG-J                          VALUE 1.     00650002
               88  UA2-ANZUZEIG-N                          VALUE 2.     00660002
                                                                        00690000
       01  ZO-DWAT0026-UA3-OUTPUT.                                      00700002
           05  ZO-DWAT0026-UA3-ANZEIGE        PIC  9(001).              00710002
               88  UA3-ANZEIGE-NOCH                        VALUE 1.     00720002
               88  UA3-ANZEIGE-ENDE                        VALUE 2.     00730002
      *                                                                 00810000
      ******* ENDE COPY-MEMBER DWAT0026 (H-MELDUNGEN BEARBEITEN) ****** 00820000
                                                                        00830000
