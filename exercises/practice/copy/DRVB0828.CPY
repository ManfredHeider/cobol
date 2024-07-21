      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0828.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0828.CPv  $
      *
      *   Rev 2.1   Jul 25 2006 13:11:46   t13ex43
      *AVB00850: PGM4 - Autom. Rückstufung von VBA-K zu VBA-S
      *
      *   Rev 2.0   Jul 20 2006 14:46:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0828                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0011                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.07.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00850!20.07.06!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB0828.
           05  PI-DRVB0828-DATEN.
               10 PI-DRVB0828-BVH-BETR-NR      PIC S9(009) COMP.
               10 PI-DRVB0828-BVH-UNTST-NR     PIC S9(009) COMP.
      *
       01  PO-DRVB0828.
           05  PO-DRVB0828-RC                  PIC  X(003).
               88 PO-DRVB0828-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0828-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0828-NI-VIVB0011.
               10 NI-DRVB0828-BVH-BETR-NR      PIC S9(004) COMP.
               10 NI-DRVB0828-BVH-UNTST-NR     PIC S9(004) COMP.
               10 NI-DRVB0828-BVH-DAT-VON      PIC S9(004) COMP.
               10 NI-DRVB0828-BVH-LAST-UPDATE  PIC S9(004) COMP.
               10 NI-DRVB0828-BVH-DAT-BIS      PIC S9(004) COMP.
               10 NI-DRVB0828-KTY-BETR-TYP     PIC S9(004) COMP.
               10 NI-DRVB0828-KNR-BETR-TYP     PIC S9(004) COMP.
               10 NI-DRVB0828-KTY-UNTST-TYP    PIC S9(004) COMP.
               10 NI-DRVB0828-KNR-UNTST-TYP    PIC S9(004) COMP.
           05  PO-DRVB0828-DCL-VIVB0011.
               10 PO-DRVB0828-BVH-BETR-NR      PIC S9(009) COMP.
               10 PO-DRVB0828-BVH-UNTST-NR     PIC S9(009) COMP.
               10 PO-DRVB0828-BVH-DAT-VON      PIC  X(026).
               10 PO-DRVB0828-BVH-LAST-UPDATE  PIC  X(026).
               10 PO-DRVB0828-BVH-DAT-BIS      PIC  X(010).
               10 PO-DRVB0828-KTY-BETR-TYP     PIC S9(004) COMP.
               10 PO-DRVB0828-KNR-BETR-TYP     PIC S9(004) COMP.
               10 PO-DRVB0828-KTY-UNTST-TYP    PIC S9(004) COMP.
               10 PO-DRVB0828-KNR-UNTST-TYP    PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0828 ****************************************
      ****
