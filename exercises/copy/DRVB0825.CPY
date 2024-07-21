      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0825.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0825.CPv  $
      *
      *   Rev 2.1   Jun 08 2006 14:48:54   t13ex43
      *AVB00850: PGM - Autom. Rückstufung von VBA-K zu VBA-S
      *
      *   Rev 2.0   Jun 01 2006 12:32:04   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0825                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0012                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.06.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00850!01.06.06!2.1 ! NEU                       ! KLEEMEIER*
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
       01  PI-DRVB0825.
           05  PI-DRVB0825-DATEN.
               10 PI-DRVB0825-MA-ST-NR         PIC S9(009) COMP.
      *
       01  PO-DRVB0825.
           05  PO-DRVB0825-RC                  PIC  X(003).
               88 PO-DRVB0825-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0825-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0825-NI-VIVB0012.
               10 NI-DRVB0825-MA-ST-NR         PIC S9(004) COMP.
               10 NI-DRVB0825-MS-NR            PIC S9(004) COMP.
               10 NI-DRVB0825-BS-NR            PIC S9(004) COMP.
               10 NI-DRVB0825-MSBZ-DAT-VON     PIC S9(004) COMP.
               10 NI-DRVB0825-MSBZ-LAST-UPDATE PIC S9(004) COMP.
               10 NI-DRVB0825-MSBZ-DAT-BIS     PIC S9(004) COMP.
           05  PO-DRVB0825-DCL-VIVB0012.
               10 PO-DRVB0825-MA-ST-NR         PIC S9(009) COMP.
               10 PO-DRVB0825-MS-NR            PIC S9(004) COMP.
               10 PO-DRVB0825-BS-NR            PIC S9(004) COMP.
               10 PO-DRVB0825-MSBZ-DAT-VON     PIC  X(026).
               10 PO-DRVB0825-MSBZ-LAST-UPDATE PIC  X(026).
               10 PO-DRVB0825-MSBZ-DAT-BIS     PIC  X(010).
      ****
      **** ENDE COPY DRVB0825 ****************************************
      ****
