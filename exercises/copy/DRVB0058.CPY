      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0058.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0058.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:14:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00001039
      *    COPY-MEMBER: DRVB0058, SCHNITTSTELLE UPVB0049             *  00002039
      *                 ERMITTELN ÄNDERUNGEN IN PCZ, BVH, AV         *  00002141
      *    --------------------------------------------------------  *  00003039
      *    ERSTELLT: GIESE                                           *  00004039
      *    DATUM:    24.06.1994                                      *  00005039
      ****************************************************************  00006039
      *    VERSION   1    01.11.94 14:33:15
      ****************************************************************  00008039
      * AENDERUNGEN:                                                 *  00009039
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009139
      * -------------------------------------------------------------*  00009239
      *         !24.06.94! 01 ! NEU                       ! GIESE    *  00009339
      *         !        !    !                           !          *  00009539
      *         !        !    !                           !          *  00009639
      *         !        !    !                           !          *  00009739
      *         !        !    !                           !          *  00009839
      *                                                              *  00009939
      * ENDE-LOGBUCH                                                 *  00010039
      ****************************************************************  00010139
      *                                                                 00042600
      *  ZEITRAUM, MIT DEM LOGTABELLE GELESEN WIRD:                     00047139
      *   PI-UPVB0049-VERARB-DAT-ANF -> ENDE-TIMESTAMP LETZTER LAUF     00047350
      *   PI-UPVB0049-VERARB-DAT-END -> ENDE-TIMESTAMP DIESES LAUFS     00047550
      *            ALSO: VERARB-DAT-ANF < LOG-SATZ <= VERARB-DAT-END    00047639
      *                                                                 00047726
      *   PI-UPVB0049-FUNKTION       FKT-OPEN  (1 MAL)                  00047806
      *                              FKT-READ  (N MAL)                  00047906
      *                              FKT-CLOSE (1 MAL)                  00048004
      *                                                                 00048104
      *   PO-UPVB0049-RC             FACHLICH AUSWERTEN !               00048239
      *                                                                 00049039
      *   PO-UPVB0049-LOG-SATZ-NR    ANZAHL GELESENE LOGSÄTZE           00049138
      *                                                                 00050004
      ***************************************************************   00051036
      *                 PARAMETER-INPUT                             *   00051136
      ***************************************************************   00051236
       01  PI-UPVB0049.                                                 00051605
           05  PI-UPVB0049-VERARB-DAT-ANF     PIC  X(26).               00051754
           05  PI-UPVB0049-VERARB-DAT-END     PIC  X(26).               00051854
           05  PI-UPVB0049-FUNKTION           PIC  X(02).               00052106
               88 PI-UPVB0049-FKT-OPEN                   VALUE 'OP'.    00052206
               88 PI-UPVB0049-FKT-READ                   VALUE 'RD'.    00052306
               88 PI-UPVB0049-FKT-CLOSE                  VALUE 'CL'.    00052406
      *                                                                 00052500
      ***************************************************************   00052636
      *                 PARAMETER-OUTPUT                            *   00052736
      ***************************************************************   00052836
       01  PO-UPVB0049.                                                 00052905
           05  PO-UPVB0049-RC                 PIC  X(03).               00053131
               88  PO-UPVB0049-RC-OK                     VALUE 'OK '.   00063032
               88  PO-UPVB0049-RC-EOF                    VALUE 'EOF'.   00063132
               88  PO-UPVB0049-RC-NOK                    VALUE 'NOK'.   00063232
                                                                        00063336
      *----------------------------------------------------------       00063436
      *    INFO-TYP: WELCHE INFORMATIONEN WERDEN ZURÜCKGELIEFERT        00063536
      *----------------------------------------------------------       00063636
           05  PO-UPVB0049-INFO-TYP           PIC  9(01).               00063736
               88  PO-VB49-INFO-TYP-PCZ                 VALUE 1.        00063836
               88  PO-VB49-INFO-TYP-BVH                 VALUE 2.        00063940
               88  PO-VB49-INFO-TYP-AV                  VALUE 3.        00064041
                                                                        00065236
      *-------------------------------------------------------------    00065336
      *    ÄNDERUNGEN IM ZUSAMMENHANG MIT BETREUERWECHSEL               00065436
      *       (ZU INFO-TYP: BVH)                                        00065545
      *-------------------------------------------------------------    00065636
           05  PO-UPVB0049-BETR-WECHSEL       PIC  9(01).               00065736
      * MA HAT BETR GEWECHSELT UND IST AKTUELL VB                       00065846
               88  PO-VB49-BETRW-VB                     VALUE 1.        00065940
      * MA HAT BETR GEWECHSELT, IST AKTUELL VM UND WAR NIE VB           00066046
               88  PO-VB49-BETRW-VM                     VALUE 2.        00066140
      * MA HAT BETR GEWECHSELT, IST AKTUELL VM UND WAR EHEMALS VB       00066246
               88  PO-VB49-BETRW-VM-EHEM-VB             VALUE 3.        00066340
                                                                        00066436
      *-------------------------------------------------------------    00066536
      *    ÄNDERUNGEN IM ZUSAMMENHANG MIT UMSTUFUNG                     00066636
      *       (ZU INFO-TYP: AV)                                         00066745
      *-------------------------------------------------------------    00066836
           05  PO-UPVB0049-UMSTUFUNG          PIC  9(01).               00066936
      * VB ZUM VM ZURÜCKGESTUFT                                         00067046
               88  PO-VB49-VB-ZURUECKGESTUFT            VALUE 1.        00067136
      * VM ZUM VB HOCHGESTUFT, WAR VORHER NIE VB                        00067246
               88  PO-VB49-VM-HOCHGESTUFT               VALUE 2.        00067340
      * VM ZUM VB HOCHGESTUFT, WAR EHEMALS VB                           00067446
               88  PO-VB49-VM-HOCHGESTUFT-EHEM-VB       VALUE 3.        00067540
                                                                        00067636
      *----------------------------------------------------------       00067701
      *    MA HAT FOLGENDEN PC NEU BEKOMMEN (EIGENER PC!)               00067846
      *       (ZU INFO-TYP: PCZ                                         00067945
      *                     BVH (AB STUFE 2)                            00068045
      *                      AV (AB STUFE 2) )                          00068145
      *----------------------------------------------------------       00068201
           05  PO-UPVB0049-PC-INFO-NEU        PIC  9(01).               00068345
               88  PO-VB49-DT-NEU                       VALUE 1.        00068446
               88  PO-VB49-NBS-NEU                      VALUE 2.        00068546
               88  PO-VB49-NBE-NEU                      VALUE 3.        00068646
               88  PO-VB49-DT-NBE-NEU                   VALUE 4.        00068746
               88  PO-VB49-FLX-NEU                      VALUE 5.        00068846
      * VB HAT KEINEN NEUEN PC BEKOMMEN                                 00068946
               88  PO-VB49-KEIN-PC-NEU                  VALUE 6.        00069046
                                                                        00069101
      *----------------------------------------------------------       00069201
      *    MA HAT FOLGENDEN PC ABGEGEBEN (EIGENER PC!)                  00069345
      *       (ZU INFO-TYP: PCZ                                         00069445
      *                     BVH (AB STUFE 2)                            00069545
      *                      AV (AB STUFE 2)  )                         00069645
      *----------------------------------------------------------       00070001
           05  PO-UPVB0049-PC-INFO-WEG        PIC  9(01).               00071046
               88  PO-VB49-DT-WEG                       VALUE 1.        00080046
               88  PO-VB49-NBS-WEG                      VALUE 2.        00090046
               88  PO-VB49-NBE-WEG                      VALUE 3.        00100046
               88  PO-VB49-DT-NBE-WEG                   VALUE 4.        00110046
               88  PO-VB49-FLX-WEG                      VALUE 5.        00120046
      * VB HAT KEINEN PC ABGEGEBEN                                      00121045
               88  PO-VB49-KEIN-PC-WEG                  VALUE 6.        00140046
                                                                        00150001
      *----------------------------------------------------------       00160001
      *    MA HAT WEITERHIN FOLGENDEN PC (EIGENER PC!)                  00170045
      *       (ZU INFO-TYP: PCZ                                         00171045
      *                     BVH (AB STUFE 2)                            00172045
      *                      AV (AB STUFE 2)  )                         00173045
      *----------------------------------------------------------       00180001
           05  PO-UPVB0049-PC-INFO-ALT-NEU    PIC  9(01).               00181045
               88  PO-VB49-DT-ALT-NEU-VORH              VALUE 1.        00200029
               88  PO-VB49-NBS-ALT-NEU-VORH             VALUE 2.        00201046
               88  PO-VB49-NBE-ALT-NEU-VORH             VALUE 3.        00210046
               88  PO-VB49-DT-NBE-ALT-NEU-VORH          VALUE 4.        00210146
               88  PO-VB49-FLX-ALT-NEU-VORH             VALUE 5.        00210346
      * VB HAT KEINEN PC, DEN ER VORHER SCHON HATTE                     00210446
               88  PO-VB49-KEIN-PC-ALT-NEU-VORH         VALUE 6.        00210546
                                                                        00210628
      *-------------------------------------------------------------    00370011
      *    PC-NUMMERN DES MA (ALS BESITZER)                             00380040
      *       (ZU INFO-TYP: PCZ                                         00381045
      *                     BVH (AB STUFE 2)                            00382045
      *                      AV (AB STUFE 2)  )                         00383045
      *-------------------------------------------------------------    00390011
           05  PO-VB49-DT-NR-ALT              PIC  9(05).               00391029
           05  PO-VB49-NBS-NR-ALT             PIC  9(05).               00391229
           05  PO-VB49-NBE-NR-ALT             PIC  9(05).               00391347
           05  PO-VB49-FLX-NR-ALT             PIC  9(05).               00391429
           05  PO-VB49-DT-NR-NEU              PIC  9(05).               00391529
           05  PO-VB49-NBS-NR-NEU             PIC  9(05).               00391729
           05  PO-VB49-NBE-NR-NEU             PIC  9(05).               00391847
           05  PO-VB49-FLX-NR-NEU             PIC  9(05).               00391929
                                                                        00392040
      *-------------------------------------------------------------    00393436
      *    DATEN ZUM VB                                                 00393536
      *       UNABHÄNGIG VON INFO-TYP                                   00393637
      *-------------------------------------------------------------    00394036
           05  PO-VB49-VBNR                   PIC  9(07).               00400016
           05  PO-VB49-VBNR-TYP               PIC  9(02).               00401048
           05  PO-VB49-VB-NAME                PIC  X(40).               00410013
           05  PO-VB49-VB-VORNAME             PIC  X(20).               00411014
      * AKTUELLER BETREUER DES VB                                       00411148
           05  PO-VB49-BETR-NR                PIC  9(07).               00412042
                                                                        00420111
      *-------------------------------------------------------------    00420236
      *    ALLGEMEINE KONTROLLINFORMATIONEN                             00420336
      *       UNABH. VON INFO-TYP                                       00420436
      *-------------------------------------------------------------    00420636
           05 PO-UPVB0049-LOG-SATZ-NR         PIC  9(03).               00420736
                                                                        00420836
                                                                        00451009
      ******** ENDE COPY-MEMBER   DRVB0058   ***********************    00452014
                                                                        00470001
