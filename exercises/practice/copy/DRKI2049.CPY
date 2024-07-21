      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2049.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2049.CPv  $
      *
      *   Rev 2.4   Sep 18 2012 15:14:38   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.3   Apr 02 2008 10:40:40   t13ex27
      *AKL00664: Allfinanz: Mandantenwechsel Programm 3
      *
      *   Rev 2.2   Aug 15 2007 11:16:48   t13ex14
      *AKI00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.0   Jul 10 2007 13:34:06   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!31.03.08! 2.2! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2049
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2049.
           05  PI-DRKI2049-KEY.
               10   PI-DRKI2049-KVBKIPERSONENID      PIC X(013).
      *
       01  PO-DRKI2049.
           05  PO-DRKI2049-RC                 PIC  X(003).
               88 PO-DRKI2049-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2049-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2049-DATEN.
               10   PO-DRKI2049-COUNTER       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
