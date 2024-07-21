      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM801S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM801S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:35:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    13.04.92 22:22:29
      *=================================================================
      * INFOLINK   06.03.87, EINZUFUEGEN
      *            IN DIE WORKING-STORAGE SECTION VON INFOHOST
      *                                               UM802,
      *            IN DIE LINKAGE SECTION         VON INFOMAN
      *                                               UM803
      *
      * ANGEPASST AM 29. MAI 1991
      *
      *-----------------------------------------------------------------
      *    U S E R - S C H N I T T S T E L L E   FUER INFOPOOLMANAGEMENT
      *-----------------------------------------------------------------
      *01  INFOLINK.
      *01  UM801S.
           05  IS-STEUERTEIL.
             10  IS-VERSION                     PIC  X(0004).
             10  IS-FUNKTION                    PIC  X(0001).
             10  IS-RC.
                 88  IS-OK                      VALUE '  '.
               15  FILLER                       PIC  X(0001).
               15  IS-RC-2                      PIC  X(0001).
           05  IS-STATUSTEIL.
             10  IS-USER-ID                     PIC  X(0008).
             10  IS-SUBFDENR                    PIC S9(0008) COMP.
             10  IS-POSITION                    PIC S9(0008) COMP.
             10  IS-DATLAENG                    PIC S9(0008) COMP.
      *        IS-DATSIZE ENTHAELT LAENGE VON IS-DATENTEIL,
      *        = AULAENGE ODER EIN VIELFACHES
==>        05  IS-DATSIZE          PIC S9(0008) COMP  VALUE +1864.
           05  IS-DATENTEIL.
      *        IS-DATEN-OCCURS-ANGABE = IS-DATSIZE
==>          10  IS-DATEN        PIC X   OCCURS 1864 TIMES.
           05  FILLER            REDEFINES      IS-DATENTEIL.
      *          IS-DE-OCCURS-ANGABE = IS-DATSIZE / 160
==>          10  IS-DE                   OCCURS 11 TIMES.
               15  IS-DENR                      PIC  9(0005).
               15  IS-DEIDENT.
                 20  IS-LIB                     PIC  X(0001).
                 20  IS-PUBLIC-ID               PIC  X(0008).
                 20  IS-DENAME.
                   25  IS-NAME                  PIC  X(0008).
                   25  IS-SUFFIX                PIC  X(0003).
                   25  IS-KEY-ERG-LAENGE        PIC  9(0002).
                   25  IS-LFDNR                 PIC  9(0004).
               15  FILLER                       PIC  X(0001).
               15  IS-MANAGEMENT-FELDER.
                 20  IS-OWNERID                 PIC  X(0008).
                 20  IS-CODE                    PIC  X(0001).
                 20  IS-FORMAT                  PIC  X(0001).
                 20  IS-STATUS                  PIC  X(0001).
                 20  IS-LOGONZAEHLER            PIC  9(0005).
                 20  IS-FILELAENGE              PIC  9(0008).
                 20  IS-ALLOCLAENGE             PIC  9(0008).
                 20  IS-LAST-UPDATE.
                   25  IS-DATUM-LAST-UPDATE.
                     30  IS-JAHR-LU             PIC  9(0002).
                     30  DE-MONAT-LU            PIC  9(0002).
                     30  IS-TAG-LU              PIC  9(0002).
                   25  IS-ZEIT-LU.
                     30  IS-STUNDE-LU           PIC  9(0002).
                     30  IS-MINUTE-LU           PIC  9(0002).
                     30  IS-SECUNDE-LU          PIC  9(0002).
                     30  IS-HSEC-LU             PIC  9(0002).
                 20  IS-LAST-READ.
                   25  IS-DATUM-LAST-READ.
                     30  IS-JAHR-LR             PIC  9(0002).
                     30  IS-MONAT-LR            PIC  9(0002).
                     30  IS-TAG-LR              PIC  9(0002).
                   25  IS-ZEIT-LR.
                     30  IS-STUNDE-LR           PIC  9(0002).
                     30  IS-MINUTE-LR           PIC  9(0002).
                     30  IS-SECUNDE-LR          PIC  9(0002).
                     30  IS-HSEC-LR             PIC  9(0002).
               15  IS-KEY-ERG                   PIC  X(0064).
               15  FILLER                       PIC  X(0004).
      *          FILLER-LAENGE = IS-DATSIZE - 160 * IS-DE-OCCURS-ANGABE
==>          10  FILLER                         PIC  X(0104).
           05  FILLER REDEFINES IS-DATENTEIL.
             10  IS-INFOPOOL-BELEGUNG.
               15  IS-AUANZAHL                  PIC  9(0008).
               15  IS-DEANZAHL                  PIC  9(0008).
               15  IS-AULAENGE                  PIC  9(0004).
               15  IS-DELAENGE                  PIC  9(0004).
               15  IS-AUBELEGT                  PIC  9(0008).
               15  IS-DEBELEGT                  PIC  9(0008).
               15  IS-VERSION-X                 PIC  X(0004).
               15  IS-DELASTNR                  PIC  9(0008).
               15  IS-ATLASTUP                  PIC  9(0008).
             10  IS-INFOPOOL-I-O-ERROR-AREA.
               15  IS-AUNR                      PIC  9(0008).
               15  IS-I-O-FUNCTION              PIC  X(0004).
               15  IS-I-O-STATUS                PIC  X(0002).
==>          10  FILLER                         PIC  X(1790).
