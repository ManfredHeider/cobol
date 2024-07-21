      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:36:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: HBILD-ID'S FUER SONDERHILFE-BILDER (DRAT0013) * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    23.04.1993                                       * 00070003
      ***************************************************************** 00090000
      *    VERSION   1    09.02.95 16:40:57
      ****************************************************************  00092000
      * AENDERUNGEN:                                                 *  00093000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00094000
      * -------------------------------------------------------------*  00095000
      *         !23.04.93! 01 ! NEU                       ! SCHÖLLH. *  00096000
      * AFVB0163!16.01.95!    ! NEU: GLOSSAR              ! SCHÖLLH. *  00100100
      *         !08.02.95! 02 ! PRODÜBERGABE              ! GIESE    *  00100300
      *         !        !    !                           !          *  00100400
      *         !        !    !                           !          *  00100500
      *         !        !    !                           !          *  00100600
      *                                                              *  00100700
      * ENDE-LOGBUCH                                                 *  00100800
      ****************************************************************  00100900
      *                                                                 00100000
      *    FUER SONDERHILFE-BILDER (Z.B. ANZEIGE DER FUNKTIONSTASTEN-   00101000
      *    BELEGUNG) WERDEN FESTE HILFEBILD-ID'S DEFINIERT.             00102000
      *    UEBER DIESE GREIFT DER HILFE-DIALOG AUF HBL (TBAT0009)       00103000
      *    UND HBLT (TBAT0010) ZU                                       00104000
      *                                                                 00105000
       01  K-HBILD-IDS.                                                 00110001
      *       FUNKTIONSTASTEN-BELEGUNG                                  00130000
           05  K-HBL-ID-HFT          PIC  X(006) VALUE 'AT0001'.        00140001
      *       GLOSSAR                                                   00130000
160195     05  K-HBL-ID-GLOSSAR      PIC  X(006) VALUE 'AT0002'.        00140001
      *                                                                 00190000
      ******** ENDE COPY-MEMBER *************************************** 00200000
                                                                        00210000
