      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:38:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00020001
      *    COPY-MEMBER: DRAT0024                                      * 00030001
      *    --------------------------------------------------------   * 00040001
      *    SCHNITTSTELLE ZU UPAT0004                                  * 00050001
      *    --------------------------------------------------------   * 00060001
      *    VERSION   01                                               * 00060111
      *    --------------------------------------------------------   * 00061011
      *    ERSTELLT: GIESE                                            * 00070001
      *    DATUM:    23.04.1993                                       * 00080010
      *    STAND:    23.04.1993                                       * 00090010
      ***************************************************************** 00100001
      *                                                                 00110001
      *                                                                 00120001
       01  PI-UPAT0004.                                                 00130001
           05  PI-UPAT0004-DATUM1.                                      00131008
             10 PI-UPAT0004-TT            PIC  X(02).                   00140006
             10 PI-UPAT0004-MM            PIC  X(02).                   00141006
             10 PI-UPAT0004-JJJJ          PIC  X(04).                   00142006
      *                                        FORMAT JJJJMMTT          00143007
           05  PI-UPAT0004-VGL-DATUM      PIC  9(08).                   00143106
      *                                                                 00143201
      *    SOLL VERGLEICH AUF DAS CICSDATUM DURCHGEFÜHRT WERDEN?J/N     00143301
           05  PI-UPAT0004-DAT-VERGLEICH PIC  X(01).                    00144106
      *                                                                 00144202
      *                                                                 00145105
       01  PO-UPAT0004.                                                 00170001
           05  PO-UPAT0004-DATUM1-OK     PIC  X(01).                    00181106
           05  PO-UPAT0004-DATUM1-ISO    PIC  X(10).                    00181206
           05  PO-UPAT0004-DATUM1-TMSTMP PIC  X(26).                    00181306
      *                                                                 00182001
      *    VGL-RC = 1  DATUM1 < VERGLEICHDATUM                          00183009
      *    VGL-RC = 2  DATUM1 = VERGLEICHDATUM                          00183109
      *    VGL-RC = 3  DATUM1 > VERGLEICHDATUM                          00183209
           05  PO-UPAT0004-VGL-RC        PIC  X(01).                    00184007
      ******** ENDE COPY-MEMBER (DRAT0024) *******************          00190001
