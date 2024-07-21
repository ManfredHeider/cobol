      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2276.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2276.CPv  $
      *
      *   Rev 2.1   Feb 24 2009 16:32:52   t13ae49
      *AKI00779: Masch. DTA-Kontrolle Badenia Programme
      *
      *   Rev 2.0   Dec 03 2008 11:58:48   t13ex14
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00779!21.01.09!2.1 ! NEU                       ! WEHLITZ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
        01  DRKI2276.
            05  GDGDATEI.
                10  GDGNAME                PIC  X(35).
                10  GDGGEN                 PIC  X(08).
            05  JOBNAME                    PIC  X(08).
            05  KEY-PRIM-PRODUKTID.
                10  GESELLSCHAFT           PIC  9(02).
                10  U-NR                   PIC  9(02).
                10  DATENID                PIC  9(02).
            05  GDG-LFDNR                  PIC S9(09) COMP.
      *****************************************************************
