      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1667.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1667.CPv  $
      *
      *   Rev 2.3   Mar 21 2016 16:18:26   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)
      *
      *   Rev 2.2   Apr 02 2004 08:46:18   t13ex49
      *Auswertung Kundenabgänge
      *
      *   Rev 2.1   Apr 02 2004 07:49:02   t13ex49
      *Übergabe Auswertung Kundenabgänge
      *
      *   Rev 2.0   Mar 29 2004 15:05:26   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1667                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1667                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00409!22.03.04!    ! Neu                       !Peter Pohl*
      *         !        !    !                           !          *
      * --------+--------+----+---------------------------+----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1667
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1667.
           05  PI-DRKI1667-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1667-VRBPERSONENID   PIC  X(0013).
             10 PI-DRKI1667-VRBPERSONENROLLEKZ   PIC  X(0001).
             10 PI-DRKI1667-VRBVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI1667.
           05  PO-DRKI1667-RC                     PIC  X(003).
               88 PO-DRKI1667-RC-OK               VALUE 'OK '.
               88 PO-DRKI1667-RC-NFD              VALUE 'NFD'.
      *
           05  PO-DRKI1667-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1667-VRBABLAUFDATUM   PIC  X(010).
             10 PO-DRKI1667-VRBAVKENNZEICHEN   PIC  X(0001).
             10 PO-DRKI1667-VRBGUELTIGKEITSDAT   PIC  X(010).
             10 PO-DRKI1667-VRBKOMMBETRKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

