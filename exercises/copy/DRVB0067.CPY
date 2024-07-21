      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0067.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0067.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:16:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0067                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SATZBESCHREIBUNG ZWISCHENDATEI EINHEITEN EING/ABG. GESCHÄFT* 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: GOECKE                                           * 00060000
      *    DATUM:    15.12.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    21.12.94 09:52:38
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0168!15.12.94! 01 ! NEU ERSTELLT              ! GOECKE   *  00140000
      *         !        !    !                           !          *  00150000
      *         !        !    !                           !          *  00160000
      *         !        !    !                           !          *  00170000
      *                                                              *  00180000
      * ENDE-LOGBUCH                                                 *  00190000
      ****************************************************************  00200000
      *                                                              *  00201000
      * DRVB0067-EINH-JAHR  NETTOEINHEITEN LFD. JAHR KUMULIERT       *  00203000
      * DRVB0067-EINH-VJAHR NETTOEINHEITEN VORJAHR   KUMULIERT       *  00203000
      * DRVB0067-ABWEICH    ABWEICHUNG EINHEITEN VOM VORJAHR         *  00203000
      * DRVB0067-ABWEICH-PROZ ABWEICHUNG PROZENTUAL VOM VORJAHR      *  00203000
      * DRVB0067-KNR-BEZREG BEZEICHNUNG DER REGION GEMÄSS EINTRAG DIR*  00203000
      * DRVB0067-VNAME      ABGEKÜRZT AUS DIRNAME IN DIR             *  00205000
      ****************************************************************  00210000
                                                                        00470000
       01  DRVB0067-REC.
           05  DRVB0067-DIR-DIR-NR      PIC  9(003).
           05  DRVB0067-DIR-NAME        PIC  X(015).
           05  DRVB0067-VNAME           PIC  X(001).
           05  DRVB0067-KNR-BEZREGION   PIC  X(010).
           05  DRVB0067-EINH-JAHR       PIC  S9(007).
           05  DRVB0067-EINH-VJAHR      PIC  S9(007).
           05  DRVB0067-ABWEICH         PIC  S9(009).
           05  DRVB0067-ABWEICH-PROZ    PIC S9(009).
      *                                                                 00450000
      ******** ENDE COPY-MEMBER (DRVB0067) *******************          00460000
