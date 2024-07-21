      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:03:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0003                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0012 MA-INFO-STRUKTUR-DATEN              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ==> DRVB0001 BEACHTEN BEI ÄNDERUNGEN!!                     * 00060000
      *    --------------------------------------------------------   * 00070000
      *    ERSTELLT: GIESE                                            * 00080000
      ***************************************************************** 00084000
      *    VERSION:  1    10.03.95 16:01:42
      ****************************************************************  00084200
      * AENDERUNGEN:                                                 *  00084300
      * AUFTRAG ! DATUM ! VER ! AENDERUNG                 ! NAME     *  00084400
      * -------------------------------------------------------------*  00084500
      *         !23.02.92! 01 ! NEU                       ! GIESE    *  00084600
      * FEVB0212!10.02.95!    ! ÄNDERUNG INFOZEILE        ! GERLACH  *  00084700
      *         !        !    ! VM-AKTIV/INAKTIV          !          *  00084800
      *                                                                 00085000
      ****************************************************************  00085105
       01  PI-UPVB0012.                                                 00086000
           05 PI-UPVB0012-MA-ST-NR       PIC S9(09) COMP.               00087000
           05 PI-UPVB0012-ST-PRZ         PIC  9(01).                    00088000
      *                                                                 00089000
       01  PO-UPVB0012.                                                 00090000
           05 PO-UPVB0012-MA-ST-NR       PIC X(07).                     00100000
           05 PO-UPVB0012-ST-PRZ         PIC X(01).                     00110000
           05 PO-UPVB0012-KNR-MA-TYP     PIC X(01).                     00120000
           05 PO-UPVB0012-MA-TYP-BEZ     PIC X(02).                     00130000
100295     05 PO-UPVB0012-MS-BEZ-KURZ.                                  00140001
100295        10 PO-UPVB0012-MS-BEZ      PIC X(04).                     00140101
100295        10 PO-UPVB0012-MS-VM-INAKT PIC X(02).                     00140202
100295*    05 PO-UPVB0012-MS-BEZ-KURZ    PIC X(04).                     00141000
           05 PO-UPVB0012-BS-NR          PIC X(01).                     00150000
           05 PO-UPVB0012-VBP-NR         PIC X(01).                     00160000
           05 PO-UPVB0012-DIR-NR         PIC X(03).                     00170000
           05 PO-UPVB0012-BETR-ST-NR     PIC X(07).                     00180000
           05 PO-UPVB0012-BETR-ST-PRZ    PIC X(01).                     00190000
           05 PO-UPVB0012-MA-NAME        PIC X(40).                     00200000
           05 PO-UPVB0012-MA-VORNAME     PIC X(40).                     00210000
100295*    05 PO-UPVB0012-MA-AKTIV       PIC X(01).                     00220000
           05 PO-UPVB0012-BETR-NAME      PIC X(40).                     00230000
           05 PO-UPVB0012-BETR-VORNAME   PIC X(40).                     00240000
           05 PO-UPVB0012-BETR-MA-TYP    PIC X(01).                     00250000
           05 PO-UPVB0012-BETR-ST-TYP    PIC X(01).                     00260000
      *                                                                 00260103
100295     05 PO-UPVB0012-INFOZL         PIC X(65).                     00261006
      *                                                                 00270000
      ******** ENDE COPY-MEMBER (DRVB0003) *******************          00280000
