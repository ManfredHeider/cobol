      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0022.CPY  $
      *    $Revision:   1.0  $
      *
      *    $Log:   L:/MF/CPY/DRVB0022.CPv  $
      *
      *   Rev 1.0   Nov 04 1995 09:07:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00015001
      *    COPY-MEMBER: DRVB0022                                      * 00020001
      *    --------------------------------------------------------   * 00030001
      *    SCHNITTSTELLE UPVB0031 PRšFEN BETREUUNGSVERHŽLTNIS         * 00040008
      *    --------------------------------------------------------   * 00042001
      *    version   01                                               * 00042109
      *    --------------------------------------------------------   * 00042209
      *    ERSTELLT: KUHN                                             * 00043001
      *    DATUM:    24.04.1993                                       * 00044009
      *    STAND:    24.04.1993                                       * 00045009
      ***************************************************************** 00046001
      *                                                                 00046107
      *  Eingabeparameter:                                              00046207
      *                                                                 00046307
      *     CICS-DAT        wird zur Pr?fung G?ltigkeitsdatum ben”tigt  00046407
      *                     Format = ISO (JJJJ-MM-TT)                   00046508
      *                                                                 00046607
      *     UNTST-NR        Stellen-Nr des Unterstellten                00046707
      *                                                                 00046807
      *     UNTST-TYP       Typ-Nr des Unterstellten                    00046907
      *                                                                 00047007
      *     UNTST-MS-NR     nur bei Untst-Typ  MA                       00047107
      *                     wird zur Pr?fung Gruppenprov.-satz ben”tigt 00047207
      *                     ansonsten ZERO                              00047307
      *                                                                 00047407
      *     UNTST-BS-NR     nur bei Untst-Typ  MA                       00047507
      *                     wird zur Pr?fung Gruppenprov.-satz ben”tigt 00047707
      *                     ansonsten ZERO                              00047807
      *                                                                 00047907
      *     UNTST-DIR       wird zur Pr?fung auf unterschiedliche Dir.  00048107
      *                     gegen Betreuer gebraucht                    00048207
      *                                                                 00048301
      *     UNTST-PROV-GP   nur bei Untst-Typ  RFT, KL und JUR          00048407
      *                     wird zur Pr?fung Gruppenprov.-satz ben”tigt 00048507
      *                     ansonsten ZERO                              00048607
      *                                                                 00048707
      *     BETR-NR         Stellen-Nr des Betreuers                    00048807
      *                                                                 00049107
      *     BETR-TYP        Typ-Nr des Betreuers                        00049207
      *                                                                 00049307
      *                                                                 00049407
      *  Ausgabeparameter:                                              00049507
      *                                                                 00049607
      *     BVH-GUELTIG     'J' falls g?ltiges BVH erkannt              00049707
      *                     'N' bei ung?ltigem BVH                      00049807
      *                                                                 00049907
      *     BETR-MS-NR      nur bei Betr-Typ  MA                        00050407
      *                     ansonsten ZERO                              00050607
      *                                                                 00050707
      *     BETR-BS-NR      nur bei Betr-Typ  MA                        00050807
      *                     ansonsten ZERO                              00050907
      *                                                                 00051007
      *     BETR-DIR        nur bei g?ltigem BVH gesetzt                00051108
      *                     ansonsten ZERO                              00051207
      *                                                                 00051307
      *     ===>   Alle 'S-' Felder nur bei g?ltigem BVH abfragen !     00051407
      *                                                                 00051507
      *     S-KUE-DAT       'J' falls K?ndigungsdatum in Zukunft gesetzt00051607
      *                     'N' falls K?ndigungsdatum offen             00051707
      *                                                                 00051807
      *     S-BESCHRAENKUNG 'J' falls UPVB0017 Beschr„nkung gefunden    00051908
      *                     'N' falls kein Beschr„nkung gefunden        00052008
      *                                                                 00052107
      *     S-DIR           'J' falls Betreuer abweichende Dir-Nr.      00052207
      *                     'N' falls Direktion gleich                  00052307
      *                                                                 00052407
       01  PI-UPVB0031.                                                 00052501
           05 PI-UPVB0031-CICS-DAT           PIC  X(10).                00052604
           05 PI-UPVB0031-UNTST-NR           PIC S9(09)    COMP.        00052705
           05 PI-UPVB0031-UNTST-TYP          PIC S9(04)    COMP.        00052805
           05 PI-UPVB0031-UNTST-MS-NR        PIC S9(04)    COMP.        00052905
           05 PI-UPVB0031-UNTST-BS-NR        PIC S9(04)    COMP.        00053005
           05 PI-UPVB0031-UNTST-DIR          PIC S9(04)    COMP.        00053105
           05 PI-UPVB0031-UNTST-PROV-GP      PIC S9(03)V99 COMP.        00053205
           05 PI-UPVB0031-BETR-NR            PIC S9(09)    COMP.        00053305
           05 PI-UPVB0031-BETR-TYP           PIC S9(04)    COMP.        00053405
      *                                                                 00054001
      *                                                                 00060001
       01  PO-UPVB0031.                                                 00060101
           05 PO-UPVB0031-BVH-GUELTIG        PIC  X(01).                00060201
           05 PO-UPVB0031-BETR-MS-NR         PIC S9(04)    COMP.        00060305
           05 PO-UPVB0031-BETR-BS-NR         PIC S9(04)    COMP.        00060405
           05 PO-UPVB0031-BETR-DIR           PIC S9(04)    COMP.        00060505
           05 PO-UPVB0031-S-KUE-DAT          PIC  X(01).                00060703
           05 PO-UPVB0031-S-BESCHRAENKUNG    PIC  X(01).                00060803
           05 PO-UPVB0031-S-DIR              PIC  X(01).                00060903
      *                                                                 00061101
      ******** ENDE COPY-MEMBER (DRVB0022) *******************          00062007
