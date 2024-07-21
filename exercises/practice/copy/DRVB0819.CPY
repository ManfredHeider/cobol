      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0819.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0819.CPv  $
      *
      *   Rev 2.1   Mar 09 2006 14:30:52   t13ae35
      *ACV00078: 2 neue CTV-Onlinebriefe für Rückstufungen
      *
      *   Rev 2.0   Mar 08 2006 16:31:48   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0819                    *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    25.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00078!08.03.06! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0819: LESEN MA-ST-NR
      *                               ERM. VSW-ZUSAGE-DATUM
      ****************************************************************
      *
       01  PI-DRVB0819.
           05  PI-DRVB0819-DATEN.
               10 PI-DRVB0819-ST-NR            PIC S9(009) COMP.
      *
       01  PO-DRVB0819.
           05  PO-DRVB0819-RC                  PIC  X(003).
               88 PO-DRVB0819-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0819-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0819-DATEN.
               10 PO-DRVB0819-VSWZ-ZUSAGE-DAT  PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

