      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0028.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAS0028.CPv  $
      *
      *   Rev 2.3   Sep 03 2010 14:30:50   t13ae36
      *ASL00351: Columbusbonus - 300EEG-Regel
      *
      *   Rev 2.2   Jul 30 2010 10:01:02   t13ae36
      *ASL00351: Columbusbonus - 300EEG-Regel
      *
      *   Rev 2.1   Jul 07 2010 14:43:50   t13ae36
      *ASL00351: Columbusbonus - 300EEG-Regel
      *
      *   Rev 2.0   Jul 01 2010 09:39:04   t13ae36
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRAS0028                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Franke                                           *
      *    DATUM:    02.07.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00351!02.07.10!    ! NEU                       ! Franke   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0028
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0028.
           05  PI-DRAS0028-VBNR            PIC S9(09) COMP VALUE ZERO.
           05  PI-DRAS0028-EEG-DAT-BIS     PIC  X(10)     VALUE SPACE.
      *
       01  PO-DRAS0028.
           05  PO-DRAS0028-RC                 PIC  X(003).
               88 PO-DRAS0028-RC-OK                VALUE 'OK '.
               88 PO-DRAS0028-RC-NOK               VALUE 'NOK'.
               88 PO-DRAS0028-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRAS0028-RC-ERR               VALUE 'ERR'.
      *
           05  PO-DRAS0028-AMPEL              PIC  X(05).
               88 PO-DRAS0028-AMPEL-ROT            VALUE 'ROT'.
               88 PO-DRAS0028-AMPEL-GRUEN          VALUE 'GRUEN'.
           05  PO-DRAS0028-EINHEITEN-EG       PIC  S9(7)V99 COMP-3.
           05  PO-DRAS0028-BES-DAT-VON        PIC  X(10).
      *
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
