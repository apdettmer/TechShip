open Unix

type founded = {
  product : Founding.product;
  funding : int;
  reputation : int;
  morale : int;
  teams : Founding.employee list list;
  investors: Founding.investor list;
  date : tm
}

let found company = 
  { product = Founding.product company;
    funding = Founding.funding company;
    reputation = Founding.reputation company;
    morale = Founding.morale company;
    teams = [];
    investors = Founding.investors company;
    date = Founding.date company
  }