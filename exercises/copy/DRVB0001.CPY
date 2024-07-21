      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:02:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0001                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    COPYSTRECKE MA-INFO-DATEN                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *      - DATEN WERDEN SO GELIEFERT, WIE AUF DER MASKE GEBRAUCHT * 00051006
      *      - VERGRÖßERUNGEN DES DATENBEREICHES ZIEHEN ANPASSUNGEN   * 00052006
      *        DES ZUGEHÖRIGEN CA-BEREICHES IM PRG NACH SICH          * 00052106
      *    --------------------------------------------------------   * 00052206
      *    VERSION   1    10.03.95 16:01:16
      *    --------------------------------------------------------   * 00054000
      *    ERSTELLT: GIESE                                            * 00055000
      *    DATUM:    23.04.1993                                       * 00056000
      *    STAND:    23.04.1993                                       * 00057000
      ***************************************************************** 00058000
      ****************************************************************  00058101
      * AENDERUNGEN:                                                 *  00058201
      * AUFTRAG ! DATUM ! VER ! AENDERUNG                 ! NAME     *  00058301
      * -------------------------------------------------------------*  00058401
      *         !23.02.92! 01 ! NEU                       ! GIESE    *  00058501
      * FEVB0212!10.02.95!    ! ÄNDERUNG INFOZEILE        ! GERLACH  *  00058601
      *         !        !    ! VM-AKTIV/INAKTIV          !          *  00058701
      ****************************************************************  00058801
      *                                                                 00059000
             15 Z-DRVB0001-ST-NR             PIC  X(07).                00060000
             15 Z-DRVB0001-ST-PRZ            PIC  X(01).                00070000
             15 Z-DRVB0001-MA-TYP-BEZ        PIC  X(02).                00080000
100295       15 Z-DRVB0001-MS-BEZ-KURZ.                                 00081004
100295          20 Z-DRVB0001-MS-BEZ         PIC  X(04).                00082004
100295          20 Z-DRVB0001-MS-VM-INAKT    PIC  X(02).                00083004
100295*      15 Z-DRVB0001-MS-BEZ-KURZ       PIC  X(04).                00090001
             15 Z-DRVB0001-MS-NR             PIC S9(04)  COMP.          00100000
             15 Z-DRVB0001-BS-NR             PIC  X(01).                00110000
             15 Z-DRVB0001-VBP-NR            PIC  X(01).                00120000
             15 Z-DRVB0001-DIR-NR            PIC  X(03).                00130000
             15 Z-DRVB0001-BETR-ST-NR        PIC  X(07).                00140000
             15 Z-DRVB0001-BETR-ST-PRZ       PIC  X(01).                00150000
             15 Z-DRVB0001-LAST-STUF-WECHSEL PIC  X(10).                00160000
      *      15 Z-DRVB0001-MA-AKTIV          PIC  X(01).                00170001
             15 Z-DRVB0001-ST-TYP-BEZ        PIC  X(02).                00180000
             15 Z-DRVB0001-ST-TYP            PIC S9(04)  COMP.          00190000
      *                                                                 00191005
100295       15 Z-DRVB0001-INFOZL            PIC  X(65).                00192005
      *                                                                 00193005
      *                                                                 00200000
      ******** ENDE COPY-MEMBER (DRVB0001) *******************          00210000
