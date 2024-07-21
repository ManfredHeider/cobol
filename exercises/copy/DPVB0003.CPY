      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:20:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0003                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    COPY-MEMBER ZUM PRÜFEN OB ORDNUNGS-BEGRIFF ÜBERGEBEN       * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHMIDT                                          * 00060000
      *    DATUM:    12.03.1993                                       * 00070000
      *    STAND:    24.05.1993                                       * 00080000
      ***************************************************************** 00090000
      *    VERSION   02   16.09.93                                   *  00090100
      ****************************************************************  00170000
      * AENDERUNGEN:                                                 *  00180000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00190000
      * -------------------------------------------------------------*  00200000
      *         !24.04.93! 01 ! NEU                       ! SCHMIDT  *  00210000
      *         !16.09.93! 02 ! SETZEN DIALOG-STATUS-INI  ! SCHOELLH.*  00220000
      *         !        !    !                           !          *  00230000
      *         !        !    !                           !          *  00240000
      *         !        !    !                           !          *  00250000
      *         !        !    !                           !          *  00260000
      *                                                              *  00270000
      * ENDE-LOGBUCH                                                 *  00280000
      ****************************************************************  00290000
      *                                                                 00100000
      *------------------------------------------------------------*    00130000
      * INPUT : CADU-OB ODER CADD-OB-VB-NR UND CADD-OB-VB-NR-P     *    00240000
      *                                                            *    00150000
      * OUTPUT: ESTNRI UND ESTPRZI                                 *    00260000
      *                                                            *    00210000
      *------------------------------------------------------------*    00220000
      *                                                                       00
      *** PRÜFEN OB ORDNUNGSBEGRIFF VORHANDEN                                 00
      *                                                                       00
           IF CADU-OB(1:7) NOT = SPACE                                        00
              MOVE CADU-OB(1:7) TO ESTNRI  IN M1I                             00
              MOVE CADU-OB(8:1) TO ESTPRZI IN M1I                             00
              MOVE K-OP-KEY     TO Z-OP-TAB-FUELLWERT                         00
              MOVE SPACE        TO CADU-OB                                    00
            ELSE                                                              00
              IF CADD-OB-VB-NR NOT = ZERO                                     00
                 MOVE CADD-OB-VB-NR     TO ESTNRI  IN M1I                     00
                 MOVE CADD-OB-VB-NR-P   TO ESTPRZI IN M1I                     00
              END-IF                                                          00
              SET DIALOG-STATUS-INI     TO TRUE
              MOVE K-OP-INI             TO Z-OP-TAB-FUELLWERT                 00
           END-IF                                                             00
      *                                                                       00
      *** OP-CODE IN OP-TAB FÜLLEN                                            00
      *                                                                       00
           PERFORM UR1-OP-TAB-FUELLEN
      *                                                                       00
      ******** ENDE COPY-MEMBER  DPVB0003 (OB UEBERGABE PRUEFEN) ****** 00340000

