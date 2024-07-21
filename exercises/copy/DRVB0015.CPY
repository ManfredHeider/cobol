      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:06:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010007
      *    COPY-MEMBER: DRVB0015                                      * 00020007
      *    --------------------------------------------------------   * 00030007
      *    SCHNITTSTELLE UPVB0024 PRÜFEN STUFE                        * 00040007
      *    --------------------------------------------------------   * 00041007
      *    ERSTELLT: GIESE                                            * 00042207
      *    DATUM:    24.04.1993                                       * 00042307
      ***************************************************************** 00042507
      *    VERSION   02   16.09.93                                   *  00042612
      ****************************************************************  00042712
      * AENDERUNGEN:                                                 *  00042812
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00042912
      * -------------------------------------------------------------*  00043012
      *         !24.04.93! 01 ! NEU                       ! GIESE    *  00043112
      *         !16.09.93! 02 ! DIFFERENZIERUNG RETCODE   ! GIESE    *  00043212
      *         !        !    !                           !          *  00043312
      *         !        !    !                           !          *  00043412
      *         !        !    !                           !          *  00043512
      *         !        !    !                           !          *  00043612
      *         !        !    !                           !          *  00043712
      *         !        !    !                           !          *  00043812
      *         !        !    !                           !          *  00043912
      *                                                              *  00044012
      * ENDE-LOGBUCH                                                 *  00044112
      ****************************************************************  00044212
      *                                                                 00044307
      *    DAS UPRO PRÜFT                                               00044411
      *    1. OB EINE GÜLTIGE STUFE (MS-BEZ-KURZ) MITGEGEBEN WURDE      00044511
      *       MA-AKTUELL-STF WIRD GEBRAUCHT, UM ZU ERKENNEN, OB         00044611
      *       HÖHER- ODER RÜCKSTUFUNG                                   00044711
      *    BEI HÖHERSTUFUNG ZUM VBA                                     00044811
      *      2. OB FÜHRUNGSZEUGNIS GÜLTIG                               00044911
      *    BEI RÜCKSTUFUNG                                              00045011
      *      2. OB ZV UND/ODER ZL VORHANDEN IST                         00045111
      *                                                                 00045211
       01  PI-UPVB0024.                                                 00045307
           05 PI-UPVB0024-MA-ST-NR              PIC S9(09) COMP.        00045407
           05 PI-UPVB0024-MA-AKTUELL-STF        PIC S9(04) COMP.        00045507
           05 PI-UPVB0024-MS-BEZ-KURZ           PIC  X(04).             00045607
           05 PI-UPVB0024-FUE-ZEUG-DAT-ISO      PIC  X(10).             00045707
           05 PI-UPVB0024-CICS-DAT-ISO          PIC  X(10).             00045807
      *                                                                 00046007
      *                                                                 00047007
       01  PO-UPVB0024.                                                 00048007
           05 PO-UPVB0024-STF-GUELTIG           PIC  X(01).             00049011
           05 PO-UPVB0024-RC-ZV-ZL              PIC 9(02).              00051011
              88 PO-UPVB0024-ZV-ZL-VORH         VALUE 1.                00053009
              88 PO-UPVB0024-ZV-VORH            VALUE 2.                00054009
              88 PO-UPVB0024-ZL-VORH            VALUE 3.                00055009
           05 PO-UPVB0024-RC-FZ                 PIC 9(02).              00055111
              88 PO-UPVB0024-FUE-ZEUG-N-VORH    VALUE 4.                00056010
              88 PO-UPVB0024-FUE-ZEUG-ZU-ALT    VALUE 5.                00057010
           05 PO-UPVB0024-MA-NEU-STF            PIC S9(04) COMP.        00060107
      *                                                                 00060207
      ******** ENDE COPY-MEMBER (DRVB0015) *******************          00060307
