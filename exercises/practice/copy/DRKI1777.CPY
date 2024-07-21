      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1777.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1777.CPv  $
      *
      *   Rev 2.1   Apr 01 2005 12:10:58   t13ex48
      *AKI00460: e-Post: Bereitstellg. der tägl. Änd. im Betreuungs
      *verhältnis
      *
      *   Rev 2.0   Mar 30 2005 09:05:46   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1777                                     *
      *    --------------------------------------------------------  *
      *    PI/PO UPRO UPKI0169                                       *
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00460!21.02.05! 1.0! NEU                       ! HSE      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1777
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRKI1777.
           05  PI-DRKI1777-DATEN.
             10 PI-DRKI1777-VB-ID              PIC X(13).

      *
       01  PO-DRKI1777.
           05  PO-DRKI1777-RC                 PIC  X(003).
               88 PO-DRKI1777-RC-OK                        VALUE 'OK '.
      * KEIN DL zur Direktion gefunden
               88 PO-DRKI1777-RC-NOT-FOUND                 VALUE 'NFD'.

           05  PO-DRKI1777-VB-NR              PIC  9(07).
           05  PO-DRKI1777-DL-NR              PIC  9(07).
           05  PO-DRKI1777-DIR-NR             PIC  9(03).
      *
      ******* ENDE COPY-MEMBER **************************************
