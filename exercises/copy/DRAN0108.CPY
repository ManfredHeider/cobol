      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0108.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0108.CPv  $
      *
      *   Rev 2.2   Apr 18 2007 20:28:08   t13ae34
      *AAN00655: monatl. Batchläufe - Teil. 2
      *
      *   Rev 2.1   Mar 18 2005 10:40:00   t13ae34
      *AKI00479: Funktion: eingereichte Einheiten zu VB in best. Ze
      *itraum
      *
      *   Rev 2.0   Mar 16 2005 11:07:44   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0108  FUNKTION OZP VSAM                  *
      *    -------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00479!16.03.05! 2.1! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!18.04.07! 2.2! 3-ST. PS                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0108                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN PRODUKTIONSERGEBNISDATEI     *
      *                           DYNAMISCH ÜBER START, READ NEXT    *
      ****************************************************************
       01 PI-DRAN0108.
          10 PI-DRAN0108-UFKT                  PIC  X.
              88 PI-DRAN0108-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRAN0108-UFKT-READ-NEXT    VALUE '2'.
              88 PI-DRAN0108-UFKT-READ-LAST    VALUE '3'.
           10  PI-DRAN0108-PKEY.
               20  PI-DRAN0108-PMANR           PIC 9(7)      COMP-3.
               20  PI-DRAN0108-PSTATDAT        PIC 9(7)      COMP-3.
180407*        20  PI-DRAN0108-PSTATSCHL       PIC 9(5)      COMP-3.
180407         20  PI-DRAN0108-PSTATSCHL.
280307             30 PI-DRAN0108-ERG-KZ          PIC 9(001).
280307             30 PI-DRAN0108-PGEB-GES        PIC 9(003) COMP-3.
280307             30 PI-DRAN0108-PROD-PROD-SCHL  PIC 9(003) COMP-3.
      *
       01 PO-DRAN0108.
          10 PO-DRAN0108-RC                    PIC  X(003).
             88 PO-DRAN0108-RC-OK              VALUE 'OK '.
             88 PO-DRAN0108-RC-NOT-FOUND       VALUE 'NFD'.
             88 PO-DRAN0108-RC-EOF             VALUE 'EOF'.

          10  PO-DRAN0108-PKEY.
              20  PO-DRAN0108-PMANR            PIC 9(7)      COMP-3.
              20  PO-DRAN0108-PSTATDAT         PIC 9(7)      COMP-3.
              20  PO-DRAN0108-PSTATSCHL        PIC 9(5)      COMP-3.

          10 PO-DRAN0108-PSATZA.
             20 PO-DRAN0108-PEIGEN.
               30 PO-DRAN0108-PEAEINH          PIC S9(8)V9   COMP-3.
               30 PO-DRAN0108-PESTCK           PIC S9(7)     COMP-3.
               30 PO-DRAN0108-PESUM            PIC S9(11)    COMP-3.
               30 PO-DRAN0108-PEBEITR          PIC S9(11)V99 COMP-3.
             20 PO-DRAN0108-PGRUPPE.
               30 PO-DRAN0108-PGRAEINH         PIC S9(8)V9   COMP-3.
               30 PO-DRAN0108-PGRSTCK          PIC S9(7)     COMP-3.
               30 PO-DRAN0108-PGRSUM           PIC S9(11)    COMP-3.
               30 PO-DRAN0108-PGRBEITR         PIC S9(11)V99 COMP-3.

