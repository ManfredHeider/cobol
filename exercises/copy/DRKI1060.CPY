      *****************************************************************
      *    COPY-MEMBER: DRKI1060                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1060                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.04.2003/13:20:33
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1060
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1060.
           05  PI-DRKI1060-KEY.


             10 PI-DRKI1060-EINKOPERSONENID   PIC X(13).
             10 PI-DRKI1060-EINKOPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1060-DATEN.


             10 PI-DRKI1060-EINKOANSPRRENTVERS   PIC X(01).
             10 PI-DRKI1060-EINKOANZMONEINKO   PIC S9(4) COMP.
             10 PI-DRKI1060-N-NKNZMNNK   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOBERTRALTVORS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKBRTRLTVRS   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOBERUNFAEVERS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKBRNFVRS   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOBERUNFWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOBETRALTWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOBREINKOJWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOEHEGARBVERTR   PIC X(01).
             10 PI-DRKI1060-EINKOEINKFTLDFRWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOEINKFTLNDFRST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKNKFTLNDFRST   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOEINKOMMENSART   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOFREIWRENTBEIT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKFRWRNTBT   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOFREIWRWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOJAHRBREINKWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOJAHRBRUTEINKO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKJHRBRTNK   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOJAHRNEINKWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOJAHRNETEINKO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKJHRNTNK   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOKINDERGELD   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-NI-EINKOKINDERGELD   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOKINDERGWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOKINDFREIBETR   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKKNDFRBTR   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOKIRCHENSTJN   PIC X(01).
             10 PI-DRKI1060-EINKOKIRCHSTEUER   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKKRCHSTR   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOLNERSATZVOR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKLNRSTZVR   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOLNERSATZVRWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOMIETEINKUEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKMTNKN   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOMIETEINKWKZ   PIC X(1).
             10 PI-DRKI1060-EINKOMONBREINKWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOMONBRUTEINKO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKMNBRTNK   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOMONNEINKWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOMONNETEINKO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKMNNTNK   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOPFLICHTBTRZUS   PIC X(01).
             10 PI-DRKI1060-EINKOSONDAGBWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOSONDAUSGBEG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKSNDSGBG   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOSONDEINBRWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOSONDEINNWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOSONDEREINKBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKSNDRNKBR   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOSONDEREINKNET   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKSNDRNKNT   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOSTEUMINAUSGA   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKSTMNSG   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOSTMINAUSWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOVERANLARTKZ   PIC X(01).
             10 PI-DRKI1060-EINKOVERSORGBEZUG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKVRSRGBZG   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOVORSBEZWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOWERBKOSTEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-NI-EINKOWERBKOSTEN   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOWERBKOWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOZINSEINKUEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKZNSNKN   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOZINSEINKWKZ   PIC X(1).
             10 PI-DRKI1060-EINKOZUBRUTEINKOJ   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKZBRTNKJ   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOZUKEINAEWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOZUKEINKOAENDD   PIC X(10).
             10 PI-DRKI1060-EINKOZUKEINKOAENDM   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKZKNKNDM   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOZUKEINKOAENDP   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKZKNKNDP   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOZUNETEINJWKZ   PIC X(01).
             10 PI-DRKI1060-EINKOZUNETEINKOJ   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKZNTNKJ   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOZUVERSTEINKO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1060-N-NKZVRSTNK   PIC S9(4) COMP.
             10 PI-DRKI1060-EINKOZUVERSTEINWKZ   PIC X(01).
             10 PI-DRKI1060-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1060.
           05  PO-DRKI1060-RC                 PIC  X(003).
               88 PO-DRKI1060-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1060-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1060-DATEN.
               10 PO-DRKI1060-PTRTABLENAME.
                  49 PO-DRKI1060-PTRTABLENAME-LEN   PIC S9(4) COMP.
                  49 PO-DRKI1060-PTRTABLENAME-TEXT  PIC X(40).
               10 PO-DRKI1060-PTRKEYDATA.
                  49 PO-DRKI1060-PTRKEYDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1060-PTRKEYDATA-TEXT    PIC X(200).
               10 PO-DRKI1060-PTRSQLDATA.
                  49 PO-DRKI1060-PTRSQLDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1060-PTRSQLDATA-TEXT    PIC X(3600).
      *
      ******* ENDE COPY-MEMBER **************************************
