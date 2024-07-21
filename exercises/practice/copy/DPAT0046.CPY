      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0046.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0046.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Aug 26 1996 09:47:00   T13EX16
      *AFPA0002 Anpassungen wegen der Workbench
      *
      *   Rev 1.0   Nov 04 1995 08:19:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00290001
      *    COPY-MEMBER: DPAT0046                                      * 00300001
      *    --------------------------------------------------------   * 00310001
      *    BATCHRAHMEN: UPRO - DATEI-FEHLER-BEHANDLUNG                * 00320001
      *    --------------------------------------------------------   * 00330001
      *    ERSTELLT: SCHMIDT                                          * 00340001
      *    DATUM:    02.05.1995                                       * 00350001
      ***************************************************************** 00360001
      *    VERSION   2    15.05.95 15:03:14
      ****************************************************************  00380001
      * AENDERUNGEN:                                                 *  00390001
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00400001
      * -------------------------------------------------------------*  00410001
      * AFVB0...!02.05.95!    ! NEU                       ! SCHMIDT  *  00420001
      * FEOP0045!12.05.95!02  !SECTION EINGEFÜGT          !          *  00440001
      *         !        !    !XR94-UPRO-DATEI-FEHLER     ! BOHLINGER*  00450001
      * AFPA0002!23.08.96! 03 ! ERR-AUFBER-NORMAL GESETZT ! AUERBACH *  00140008
      *         !        !    ! WEGEN MF WB               !          *  00142000
      *         !        !    !                           !          *  00460001
      *         !        !    !                           !          *  00470001
      *                                                              *  00480001
      * ENDE-LOGBUCH                                                 *  00490001
      ****************************************************************  00500001
      *                                                                 00501001
      *    FEHLERBEHANDLUNGS-ROUTINE, DIE IN EINEM UPRO BEI AUFTRETEN   00502001
      *    EINES DATEI-FEHLERS BENOETIGT WIRD                           00503001
      *                                                                 00504001
      *------------------------------------------------------------*    00505001
      * XR94-UPRO-DATEI-FEHLER                                     *    00506001
      *------------------------------------------------------------*    00507001
      * INPUT : MUSS: S-DATEI-STATUS, K-PGM-NAME                   *    00507101
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 /04*    00507201
      * OUTPUT: GEFUELLTER ERR-ERROR-BEREICH                       *    00509101
      * VERARBEITUNG:                                              *    00509301
      *   - FUELLEN DER ERROR-SCHNITTSTELLE FUER UPRO-SQL-FEHLER   *    00509401
      *------------------------------------------------------------*    00520001
950512 XR94-UPRO-DATEI-FEHLER SECTION.                                  00530001
950512 XR94-ANF.                                                        00540001
      *                                                                 00630001
      *    * VERSORGEN SCHNITTSTELLE                                    00640001
950512     SET ERR-RC-SCHWERER-FEHLER      TO TRUE                      00650001
950512     SET ERR-FTYP-DATEI              TO TRUE                      00651001
960823     SET ERR-AUFBER-NORMAL           TO TRUE                      00651001
950512     MOVE K-PGM-NAME                 TO ERR-UPRONAME              00660001
950512     MOVE S-DATEI-STATUS             TO ERR-FILE-STATUS           00670001
950512     .                                                            00690001
950512 XR94-EXIT.                                                       00700001
950512     EXIT.                                                        00710001
      *                                                                 00720001
      *------------------------------------------------------------*    00505001
      * XR94-DATEI-FEHLER                                          *    00506001
      *                                                            *    00506001
      * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                     *    00506001
      * ACHTUNG: DIESE SECTION NICHT EINSETZEN                     *    00506001
      *          (WIRD NOCH BENÖTIGT IN UPVB0062)                  *    00506001
      *          (WIRD EVTL. DEMNÄCHST GELÖSCHT !!!)               *    00506001
      *                                                            *    00506001
      *------------------------------------------------------------*    00507001
      * INPUT : MUSS: S-DATEI-STATUS, K-PGM-NAME                   *    00507101
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 /04*    00507201
      * OUTPUT: GEFUELLTER ERR-ERROR-BEREICH                       *    00509101
      * VERARBEITUNG:                                              *    00509301
      *   - FUELLEN DER ERROR-SCHNITTSTELLE FUER UPRO-SQL-FEHLER   *    00509401
      *------------------------------------------------------------*    00520001
       XR94-DATEI-FEHLER SECTION.                                       00530001
       XR94-D-ANF.                                                      00540001
      *                                                                 00630001
      *    * VERSORGEN SCHNITTSTELLE                                    00640001
           SET ERR-RC-SCHWERER-FEHLER      TO TRUE                      00650001
           SET ERR-FTYP-DATEI              TO TRUE                      00651001
           MOVE K-PGM-NAME                 TO ERR-UPRONAME              00660001
           MOVE S-DATEI-STATUS             TO ERR-FILE-STATUS           00670001
           .                                                            00690001
       XR94-D-EXIT.                                                     00700001
           EXIT.                                                        00710001
      *                                                                 00504001
      ******** ENDE COPY-MEMBER (DPAT0046) *******************          00730001
                                                                        00740001
